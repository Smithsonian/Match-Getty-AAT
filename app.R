#Load packages ----
library(shiny)
library(DT)
library(dplyr)
library(stringr)
library(jsonlite)
library(futile.logger)
library(RSQLite)
library(shinyWidgets)
library(shinycssloaders)
library(parallel)
library(WriteXLS)
library(openxlsx)



#Settings----
app_name <- "Match Getty AAT"
app_ver <- "0.5.2"
github_link <- "https://github.com/Smithsonian/Match-Getty-AAT"
csv_database <- paste0("data/csv_", format(Sys.time(), "%Y%m%d_%H%M%S_"), (runif(1) * 10000), ".sqlite3")
options(stringsAsFactors = FALSE)
options(encoding = 'UTF-8')

#How many parallel queries to run, 
# too many will cause errors
# 4 seems to be a top limit
no_cores <- 2


#load functions
source("functions.R")



#UI----
ui <- fluidPage(
  
  # App title
  titlePanel(app_name),
  tabsetPanel(type = "tabs",
      tabPanel("Step 1 - Automatic Matching",
         br(),
         fluidRow(
           column(width = 4,
                  uiOutput("main")
           ),
           column(width = 4,
                  uiOutput("steps")
           ),
           column(width = 4, 
                  uiOutput("uploadcsv"),
                  uiOutput("error_msg"),
                  uiOutput("inputfile")
           )
         ),
         fluidRow(
           column(width = 8, 
                  withSpinner(DT::dataTableOutput("step1table"))
           ),
           column(width = 4,
                  withSpinner(uiOutput("step1aatresult"))
           )
         )
      ),
      tabPanel("Step 2 - Manual Matching",
          br(),
          fluidRow(
            column(width = 4, 
                   br(),
                   uiOutput("step1_msg"),
                   uiOutput("choose_string"),
                   uiOutput("chosen_string")
            ),
            column(width = 8,
                   uiOutput("step2detail")
            )
          ),
          DT::dataTableOutput("step2table")
      ),
      tabPanel("Step 3 - Download Results",
         br(),
         fluidRow(
             column(width = 4, 
                    br(),
                    uiOutput("step1_msg2"),
                    uiOutput("downloadData"),
                    br(),
                    br()
                )
           )
      )
  ),
  hr(),
  #footer ----
  HTML(paste0("<br><br><br><div class=\"footer navbar-fixed-bottom\" style=\"background: #FFFFFF;\"><br><p>&nbsp;&nbsp;<a href=\"http://dpo.si.edu\" target = _blank><img src=\"dpologo.jpg\"></a> | ", app_name, ", ver. ", app_ver, " | <a href=\"", github_link, "\" target = _blank>Source code</a></p></div>"))
)




server <- function(input, output, session) {
  
  #Setup Logging
  logfile <- paste0("logs/", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
  dir.create('logs', showWarnings = FALSE)
  flog.logger("getty", INFO, appender=appender.file(logfile))
  
  
  #inputfile----
  output$inputfile <- renderUI({
    if (is.null(input$csvinput)){
      shinyWidgets::panel(
        uiOutput("csv_info"),
        heading = "Input File",
        status = "success"
      )
    }
  })
  
  
  #csv_info----
  output$csv_info <- renderUI({
    HTML("<p>To use this app, upload a CSV or Excel file.
          <ul>
            <li>If the input file is a CSV file (.csv), the file must be comma-separated</li>
            <li>If the input file is an Excel file (.xlsx), only the first sheet is used</li>
          </ul>
          <p>The input file must be encoded using UTF-8 and have at least these 2 columns:</p>
          <ul>
             <li><samp>id</samp> - ID for the row</li>
             <li><samp>term</samp> - term to match to the AAT</li>
          </ul>
          <p>Two columns are optional:</p>
          <ul>
             <li><samp>keywords</samp> - words or phrases that describe the item, which are used to rate the matching terms</li>
             <li><samp>linked_aat_term</samp> - AAT term from previous efforts</li>
          </ul>
          <p>Any other columns in the input file will be ignored but returned in the results file.</p>")
  })
  
  
  #main----
  output$main <- renderUI({
    if (is.null(input$csvinput)){
      shinyWidgets::panel(
        p("Match Getty AAT is a prototype app that matches terms in a file to the ", tags$strong("Getty Art & Architecture Thesaurus"), "using their Linked Open Data portal."),
        HTML("<p><img src=\"header_aat_main.gif\"></p>"),
        p("The app tries to find the best match by using a set of keywords included with each row, when available, to try to disambiguate the usage. For terms where many matches are found, the app allows the user to select the best one. Once the process is completed, the results file can be downloaded for further processing or importing to the CIS or other database."),
        HTML("<p>This app was made by the Digitization Program Office, OCIO.</p><p>The AAT is queried using their Linked Open Data SPARQL endpoint: <a href=\"http://vocab.getty.edu/queries\">http://vocab.getty.edu/queries</a></p>"),
        heading = "Welcome",
        status = "primary"
      )
    }
  })
  
  
  #steps----
  output$steps <- renderUI({
    if (is.null(input$csvinput)){
      shinyWidgets::panel(
        HTML("<p>This app will take the string in the column
                            \"term\" and match it with the The Art & Architecture
                            Thesaurus using their Linked Open Data portal.</p>
                            <p>Steps:</p>
                             <ol>
                               <li>Batch Matching of Subjects: Automated match 
                                  based on the term and keywords.</li>
                               <li>Manual Matching of Subjects: Select the 
                                  appropiate match by executing a full text search.</li>
                                <li>Download Results: Download a file with the rows from 
                                  the input file with the match from the AAT.</li>
                             </ol>"),
        heading = "Steps",
        status = "info"
      )
    }
  })
  
  
  
  #uploadcsv ----
  output$uploadcsv <- renderUI({
    if (is.null(input$csvinput)){
      tagList(
        fileInput("csvinput", "Upload an Input File",
            multiple = FALSE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv",
                       "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                       ".xlsx"), 
            width = "100%")
      )
    }
  })
  
  
  #step1table----
  output$step1table <- DT::renderDataTable({
    req(input$csvinput)
    
    #Read Upload
    filename_to_check <- input$csvinput$name
    ext_to_check <- stringr::str_split(filename_to_check, '[.]')[[1]]
    ext_to_check <- ext_to_check[length(ext_to_check)]
    
    if (ext_to_check == "csv"){
      #Read CSV file----
      csvinput <- read.csv(input$csvinput$datapath, header = TRUE, stringsAsFactors = FALSE)
      
      # Process any error messages
      if (class(csvinput) == "try-error"){
        flog.error(paste0("Error reading CSV: ", filename_to_check), name = "csv")
        output$error_msg <- renderUI({
          HTML(paste0("<br><div class=\"alert alert-danger\" role=\"alert\">File ", filename_to_check, " does not appear to be a valid file. Please reload the application and try again.</div>"))
        })
        req(FALSE)
      }else{
        #Data to sqlite
        csv_to_sqlite(csv_database, csvinput)
      }
    }else if (ext_to_check == "xlsx"){
      #Read XLSX file----
      try(csvinput <- openxlsx::read.xlsx(input$csvinput$datapath, sheet = 1, check.names = TRUE), silent = TRUE)
      
      if (exists("csvinput") == FALSE){
        flog.error(paste0("Error reading Excel: ", filename_to_check), name = "xlsx")
        output$error_msg <- renderUI({
          HTML(paste0("<br><div class=\"alert alert-danger\" role=\"alert\">File ", filename_to_check, " does not appear to be a valid file. Please reload the application and try again.</div>"))
        })
        req(FALSE)
      }else{
        #Data to sqlite
        csv_to_sqlite(csv_database, csvinput)
      }
    }else{
      #Some other file or there was a problem
      flog.error(paste0("Error reading file: ", filename_to_check), name = "csv")
      output$error_msg <- renderUI({
        HTML("<br><div class=\"alert alert-danger\" role=\"alert\">File must be a valid and have the extension csv or xlsx. Please reload the application and try again.</div>")
      })
      req(FALSE)
    }
    
    #No errors
    output$error_msg <- renderUI({HTML("&nbsp;")})
    
    #Open database
    csvinput_db <- dbConnect(RSQLite::SQLite(), csv_database)
    this_query <- paste0("PRAGMA table_info(csv);")
    flog.info(paste0("database: ", csv_database), name = "locations")
    table_info <- dbGetQuery(csvinput_db, this_query)$name
    
    #Optional columns
    if ("keywords" %in% table_info == FALSE){
      this_query <- paste0("ALTER TABLE csv ADD COLUMN keywords TEXT;")
      flog.info(paste0("this_query: ", this_query), name = "locations")
      n <- dbSendQuery(csvinput_db, this_query)
      keywords_field <<- FALSE
    }else{
      keywords_field <<- TRUE
    }
    
    if ("linked_aat_term" %in% table_info == FALSE){
      this_query <- paste0("ALTER TABLE csv ADD COLUMN linked_aat_term TEXT;")
      flog.info(paste0("this_query: ", this_query), name = "locations")
      n <- dbSendQuery(csvinput_db, this_query)
      linked_field <<- FALSE
    }else{
      linked_field <<- TRUE
    }
    
    this_query <- paste0("SELECT rowid, term, linked_aat_term, keywords FROM csv")
    flog.info(paste0("this_query: ", this_query), name = "locations")
    all_rows <- dbGetQuery(csvinput_db, this_query)
    
    progress_val <- 0.01
    progress0 <- shiny::Progress$new()
    progress0$set(message = "Initializing. Please wait...", value = progress_val)
    on.exit(progress0$close())
    
    no_rows <- dim(all_rows)[1]
    
    #parallel ----
    # Calculate the number of cores, if not in settings
    if (!exists("no_cores")){
      no_cores <- detectCores() - 1
    }
    # Initiate cluster
    cl <- makeCluster(no_cores)
    
    #Export data to cluster
    clusterExport(cl=cl, varlist=c("all_rows", "logfile", "csv_database"), envir=environment())
    
    #Divide into steps
    step_grouping <- no_cores * 3
    steps <- ceiling(dim(all_rows)[1] / step_grouping)
    
    progress_steps <- round(((0.9) / steps), 4)
    progress_val <- 0.1
    progress0$set(value = progress_val, message = "Querying AAT...")
    
    results <- list()
    
    #Run each batch, let the user know of the progress
    for (s in seq(1, steps)){
      to_row <- s * step_grouping
      from_row <- to_row - step_grouping
      if (to_row > no_rows){
        to_row <- no_rows
      }
      
      if (from_row == 0){
        from_row <- 1
      }
      
      res <- parLapply(cl, seq(from_row, to_row), find_aat)
      progress_val <- (s * progress_steps) + 0.1
      progress0$set(value = progress_val, message = paste0("Querying AAT (", round(((s/steps) * 100), 1), "% completed)"))
      results <- c(results, res)
    }
    
    #stop cluster
    stopCluster(cl)
    #progress0$set(message = "Done! Saving results...", value = 1)
    progress0$set(message = "Done!", value = 1)
    
    res_df <- data.frame(matrix(ncol = 4, nrow = 0, data = NA))
    for (i in seq(1, length(results))){
      data <- data.frame(results[[i]])
      res_df <- rbind(res_df, data)
    }
    
    names(res_df) = c("csv_rowid", "aat_term", "aat_id", "aat_note")
    res_df <- dplyr::filter(res_df, !is.na(aat_id))
    res_df <- dplyr::distinct(res_df)
    
    #Add table of results
    dbWriteTable(csvinput_db, "matches", res_df, overwrite = TRUE)
    n <- dbExecute(csvinput_db, "CREATE INDEX matches_csv_rowid_idx ON matches(csv_rowid)")
    n <- dbExecute(csvinput_db, "CREATE INDEX matches_aat_id_idx ON matches(aat_id)")
    n <- dbExecute(csvinput_db, "CREATE INDEX matches_aat_term_idx ON matches(aat_term)")
    n <- dbExecute(csvinput_db, "ALTER TABLE matches ADD COLUMN selected INTEGER DEFAULT 0")
    
    n <- dbExecute(csvinput_db, "UPDATE matches SET selected = 1 WHERE csv_rowid in (SELECT c.rowid FROM csv c LEFT JOIN (SELECT csv_rowid, COUNT(*) AS no_matches FROM matches GROUP BY csv_rowid) m1 ON c.rowid = m1.csv_rowid WHERE no_matches = 1)")
    
    #Get fresh version of table to display
    this_query <- paste0("SELECT c.rowid, c.*, m.*, REPLACE(m.aat_id, 'aat:', '') as aat_id_int, COALESCE(m1.no_matches, 0) as no_matches, COALESCE(m1.no_matches, 0) as 'Number of matches' FROM csv c LEFT JOIN (SELECT csv_rowid, COUNT(*) AS no_matches FROM matches GROUP BY csv_rowid) m1 ON c.rowid = m1.csv_rowid LEFT JOIN matches m ON m1.csv_rowid = m.csv_rowid AND (m1.no_matches == 1 OR m1.no_matches IS NULL)")
    flog.info(paste0("this_query: ", this_query), name = "locations")
    
    # resultsdf1 <- dbGetQuery(csvinput_db, this_query)
    # resultsdf <<- dplyr::select(resultsdf1, -aat_id_int)
    resultsdf <<- dbGetQuery(csvinput_db, this_query)
    
    total_rows <- dim(resultsdf)[1]
    
    #How many matches?
    this_query <- paste0("SELECT COUNT(*) as no_matches FROM csv WHERE rowid IN (SELECT DISTINCT csv_rowid FROM matches)")
    flog.info(paste0("this_query: ", this_query), name = "locations")
    no_matches <- dbGetQuery(csvinput_db, this_query)$no_matches
    
    dbDisconnect(csvinput_db)
    
    results_table <- dplyr::select(resultsdf, -aat_note)
    results_table <- dplyr::select(results_table, -csv_rowid)
    results_table <- dplyr::select(results_table, -rowid)
    results_table <- dplyr::select(results_table, -aat_term)
    results_table <- dplyr::select(results_table, -aat_id)
    results_table <- dplyr::select(results_table, -aat_id_int)
    results_table <- dplyr::select(results_table, -no_matches)
    results_table <- dplyr::select(results_table, -selected)
    
    if (keywords_field == FALSE){
      results_table <- dplyr::select(results_table, -keywords)
    }
    
    if (linked_field == FALSE){
      results_table <- dplyr::select(results_table, -linked_aat_term)
    }
    
    no_cols <- dim(results_table)[2]
    
    no_matches_vals <- unique(resultsdf$no_matches)
    no_matches_vals <- no_matches_vals[!no_matches_vals %in% 0:1]
    no_matches_vals_colors <- rep('#fcf8e3', length(no_matches_vals))
    
    DT::datatable(results_table, 
                  caption = paste0('Found ', no_matches, ' matches automatically in the AAT (of ', total_rows, ', ', round((no_matches/total_rows) * 100, 2), '%)'), 
                  escape = FALSE, 
                  options = list(searching = TRUE, 
                                 ordering = TRUE, 
                                 pageLength = 10, 
                                 paging = TRUE), 
                  rownames = FALSE, 
                  selection = 'single')  %>% DT::formatStyle(
                    no_cols,
                    backgroundColor = DT::styleEqual(c(0, 1, no_matches_vals), c('#f2dede', '#dff0d8', no_matches_vals_colors)),
                  )
  })
  
  
  #step1aatresult ----
  output$step1aatresult <- renderUI({
    #Display the AAT Subject and ID of the selected row
    req(input$step1table_rows_selected)
    
    res <- resultsdf[input$step1table_rows_selected, ]
    
    csvinput_db <- dbConnect(RSQLite::SQLite(), csv_database)
    
    this_query <- paste0("SELECT aat_note FROM matches WHERE csv_rowid = ", res$rowid)
    flog.info(paste0("this_query: ", this_query), name = "locations")
    note <- dbGetQuery(csvinput_db, this_query)
    
    dbDisconnect(csvinput_db)
    
    if (dim(note)[1] == 1){
      
      AAT_term_notes <- note$aat_note[1]
      
      tagList(
        HTML("<div class=\"panel panel-success\">
              <div class=\"panel-heading\">
                <h3 class=\"panel-title\">Match found</h3>
              </div>
              <div class=\"panel-body\">"),
        HTML(paste0("<dl><dt>Term</dt><dd>", res$aat_term, 
                    "</dd><dt>AAT ID</dt><dd>", res$aat_id_int, " ",
                    actionLink("showaat2", label = "", icon = icon("info-sign", lib = "glyphicon"), title = "AAT page for this term", alt = "AAT page for this term"))),
        HTML(paste0("</dd><dt>Notes</dt><dd>", AAT_term_notes, "</dd></dl>")),
        HTML("</div></div>")
      )
    }else if (dim(note)[1] > 1){
      HTML("<div class=\"panel panel-warning\">
            <div class=\"panel-heading\"> 
              <h3 class=\"panel-title\">Could not find a single match</h3> 
            </div> 
            <div class=\"panel-body\">
              <p>The app found many possible matches for this row automatically.</p>
              <p>In <code>Step 2</code> you can select the best match manually.</p>
            </div>
          </div>")
    }else{
        HTML("<div class=\"panel panel-danger\">
            <div class=\"panel-heading\"> 
              <h3 class=\"panel-title\">Could not find match</h3> 
            </div> 
            <div class=\"panel-body\">
              <p>The app could not find a match for this row.</p>
            </div>
          </div>")
    }
  })
  
  
  #choose_string----
  output$choose_string <- renderUI({
    output$step1_msg <- renderUI({
      HTML("<br>
          <div class=\"alert alert-info\" role=\"alert\">Upload a file in Step 1</div>")
    })

    req(input$csvinput)
    
    output$step1_msg <- renderUI({HTML("")})
    
    csvinput_db <- dbConnect(RSQLite::SQLite(), csv_database)
    csvinput1 <- dbGetQuery(csvinput_db, "WITH data AS (SELECT csv_rowid, count(*) as no_matches FROM matches GROUP BY csv_rowid) SELECT c.rowid, c.* FROM csv c, data d WHERE c.rowid = d.csv_rowid AND d.no_matches > 1")
    dbDisconnect(csvinput_db)
    
    choices <- data.frame(csvinput1$rowid, paste0(csvinput1$term, " (", csvinput1$id, ")"))
    names(choices) <- c("rowid", "row")
    obj_list <- as.list(choices$rowid)
    names(obj_list) <- choices$row
    
    tagList(
      HTML(paste0("The app was unable to find a match automatically for ", dim(csvinput1)[1], " rows. Use this step to manually select the best AAT term for these rows.")),
      br(),
      selectInput(inputId = "row", label = "Select a row:", choices = obj_list, width = "100%", multiple = FALSE, selectize = FALSE)
    )
  })
  
  
  
  #step2table----
  output$step2table <- DT::renderDataTable({
    req(input$csvinput)
    req(input$row)
    
    this_query <- paste0("SELECT * FROM matches WHERE csv_rowid = ", input$row)
    flog.info(paste0("this_query: ", this_query), name = "locations")
    csvinput_db <- dbConnect(RSQLite::SQLite(), csv_database)
    results <<- dbGetQuery(csvinput_db, this_query)
    dbDisconnect(csvinput_db)
    
    results_table <- dplyr::select(results, -csv_rowid)
    results_table <- dplyr::select(results_table, -selected)
    
    DT::datatable(results_table, 
                  escape = FALSE, 
                  options = list(searching = TRUE, 
                                 ordering = TRUE, 
                                 pageLength = 15, 
                                 paging = TRUE, 
                                 language = list(zeroRecords = "No matches found")
                  ), 
                  rownames = FALSE, 
                  selection = 'single',
                  caption = "Select a subject to match") #%>% formatStyle("term", "white-space" = "nowrap") 
                  #%>% formatStyle("parents", "white-space" = "nowrap") 
  })
  
  
  #step2detail ----
  output$step2detail <- renderUI({
    req(input$csvinput)
    req(input$row)
    
    csvinput_db <- dbConnect(RSQLite::SQLite(), csv_database)
    has_term <- dbGetQuery(csvinput_db, paste0("SELECT aat_term, aat_id FROM matches WHERE csv_rowid = ", input$row, " AND selected = 1"))
    dbDisconnect(csvinput_db)

    if (dim(has_term)[1] > 0){
      output$chosen_string <- renderUI({HTML(paste0("<br><div class=\"alert alert-success\" role=\"alert\">Term saved for this object: ", has_term$aat_term, " (", has_term$aat_id, ")</div>"))})
    }else{
      output$chosen_string <- renderUI({HTML("&nbsp;")})
    }
    
    if (!is.null(input$step2table_rows_selected)){
      
      res <- results[input$step2table_rows_selected, ]
      
      AAT_url <- paste0("http://vocab.getty.edu/page/aat/", res$id)
      
      csvinput_db <- dbConnect(RSQLite::SQLite(), csv_database)
      this_row <- dbGetQuery(csvinput_db, paste0("SELECT * FROM csv WHERE rowid = ", input$row))
      this_row_term <- this_row$term
      this_row_id <- this_row$id

      dbDisconnect(csvinput_db)
      
      tagList(
        HTML("<div class=\"panel panel-success\"> <div class=\"panel-heading\"> <h3 class=\"panel-title\">Result selected</h3> </div> <div class=\"panel-body\">"),
        HTML(paste0("<dl class=\"dl-horizontal\"><dt>ID</dt><dd>", this_row_id, "</dd><dt>Term</dt><dd>", res$aat_term, "</dd><dt>AAT ID</dt><dd>", res$aat_id)),
        actionLink("showaat", label = "", icon = icon("info-sign", lib = "glyphicon"), title = "AAT page for this term", alt = "AAT page for this term"),
        HTML("</dd></dl></p>"),
        # Save button
        actionButton("saverow", paste0("Save this AAT term for item ID ", this_row_id), class = "btn btn-primary", icon = icon("ok", lib = "glyphicon")),
        HTML("</div></div>")
      )
    }else{
      tagList(
        HTML("<div class=\"panel panel-success\"> 
                <div class=\"panel-heading\"> 
                  <h3 class=\"panel-title\">Result selected</h3> 
                </div> 
                <div class=\"panel-body\">
                  <br>
                  <em>Select a match from the table below.</em>
                  <br>
                </div>
              </div>")
      )
    }
  })
  
  
  #showaat----
  observeEvent(input$showaat, {
    req(input$step2table_rows_selected)
    
    res <- results[input$step2table_rows_selected, ]
    
    AAT_url <- paste0("http://vocab.getty.edu/page/aat/", res$id)
    
    showModal(modalDialog(
      title = "AAT Term Info",
      tags$iframe(src = AAT_url, height = 650, width = 860),
      easyClose = TRUE,
      footer = modalButton("Close", icon = icon("close")),
      size = "l")
    )
  })
  
  observeEvent(input$showaat2, {
    req(input$step1table_rows_selected)
    
    res <- resultsdf[input$step1table_rows_selected, ]
    
    AAT_url <- stringr::str_replace(res$aat_id, "aat:", "http://vocab.getty.edu/page/aat/")
    
    showModal(modalDialog(
      title = "AAT Term Info",
      tags$iframe(src = AAT_url, height = 650, width = 860),
      easyClose = TRUE,
      footer = modalButton("Close", icon = icon("close")),
      size = "l")
    )
  })
  
 
  #saverow, observe----
  observeEvent(input$saverow, {
    req(input$csvinput)
    req(input$row)
    req(input$step2table_rows_selected)
    
    selected_row <- results[input$step2table_rows_selected, ]
    
    csvinput_db <- dbConnect(RSQLite::SQLite(), csv_database)
    query <- paste0("UPDATE matches SET selected = 1 WHERE csv_rowid = ", input$row, " AND aat_id = '", selected_row$aat_id, "'")
    flog.info(paste0("update query: ", query), name = "getty")
    n <- dbExecute(csvinput_db, query)
    dbDisconnect(csvinput_db)
    output$chosen_string <- renderUI({
      HTML(paste0("<br><div class=\"alert alert-success\" role=\"alert\">Term saved for this object: ", selected_row$aat_term, " (", selected_row$aat_id, ")</div>"))
    })
  })
  
  
  
  #downloadcsv1----
  #Download CSV
  output$downloadcsv1 <- downloadHandler(
    #Downloadable csv of results
    filename = function() {
      paste("results_aat_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      csvinput_db <- dbConnect(RSQLite::SQLite(), csv_database)
      data <- dbGetQuery(csvinput_db, "SELECT c.*, m.aat_id, aat_term, aat_note FROM csv c LEFT JOIN matches m ON c.rowid = m.csv_rowid and m.selected = 1")
      dbDisconnect(csvinput_db)
      
      #Drop unused columns
      if (keywords_field == FALSE){
        data <- dplyr::select(data, -keywords)
      }
      if (linked_field == FALSE){
        data <- dplyr::select(data, -linked_aat_term)
      }
      
      write.csv(data, file, quote = TRUE, na = "", row.names = FALSE)
    }
  )
  
  
  #downloadcsv2----
  #Download XLSX
  output$downloadcsv2 <- downloadHandler(
    filename = function(){paste0("results_aat_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")},
    
    content = function(file){
      csvinput_db <- dbConnect(RSQLite::SQLite(), csv_database)
      data <- dbGetQuery(csvinput_db, "SELECT c.*, m.aat_id, aat_term, aat_note FROM csv c LEFT JOIN matches m ON c.rowid = m.csv_rowid and m.selected = 1")
      dbDisconnect(csvinput_db)
      
      #Drop unused columns
      if (keywords_field == FALSE){
        data <- dplyr::select(data, -keywords)
      }
      if (linked_field == FALSE){
        data <- dplyr::select(data, -linked_aat_term)
      }
      
      WriteXLS::WriteXLS(x = data, ExcelFileName = file, AdjWidth = TRUE, BoldHeaderRow = TRUE, Encoding = "UTF-8", row.names = FALSE, FreezeRow = 1, SheetNames = c("results_aat"))
    },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  
  #downloadData ----
  output$downloadData <- renderUI({
    output$step1_msg2 <- renderUI({
      HTML("<br><div class=\"alert alert-info\" role=\"alert\">Upload a file in Step 1</div>")
    })
    
    req(input$csvinput)
    
    output$step1_msg2 <- renderUI({HTML("&nbsp;")})
    
    shinyWidgets::panel(
      HTML("<p>Download the results as a Comma Separated Values file (.csv) or an Excel file (.xlsx).</p><p>The results file contains the same columns as the input file, untouched, with three additional columns:</p>"),
      HTML("<ul><li>aat_id - ID of the term matched</li><li>aat_term - Term matched</li><li>aat_note - Usage note for the term matched</li></ul>"),
      br(),
      HTML("<div class=\"btn-toolbar\">"),
      downloadButton("downloadcsv1", "CSV (.csv)", class = "btn-success"),
      downloadButton("downloadcsv2", "Excel (.xlsx)", class = "btn-primary"),
      HTML("</div>"),
      heading = "Download Results",
      status = "primary"
    )
  })
}



#Run app----
shinyApp(ui = ui, server = server, onStart = function() {
  cat("Loading\n")
  #Cleanup on closing
  onStop(function() {
    cat("Closing\n")
    try(dbDisconnect(csvinput_db), silent = TRUE)
    #try(unlink(csv_database), silent = TRUE)
  })
})
