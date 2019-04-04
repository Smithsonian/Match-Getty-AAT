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
app_ver <- "0.4.0"
github_link <- "https://github.com/Smithsonian/Match-Getty-AAT"
csv_database <- paste0("data/csv_", format(Sys.time(), "%Y%m%d%H%M%S"), ".sqlite3")
options(stringsAsFactors = FALSE)
options(encoding = 'UTF-8')
#Logfile
logfile <- paste0("logs/", format(Sys.time(), "%Y%m%d_%H%M%S"), ".txt")
#load functions
source("functions.R")


#UI----
ui <- fluidPage(
  
  # App title
  titlePanel(app_name),
  tabsetPanel(type = "tabs",
      tabPanel("1 - Batch Matching of Subjects",
         br(),
         fluidRow(
           column(width = 4, 
                  uiOutput("main")
           ),
           column(width = 4, 
                  uiOutput("inputfile")
           ),
           column(width = 4, 
                  uiOutput("loadcsv"),
                  uiOutput("error_msg")
           )
         ),
         fluidRow(
           column(width = 8, 
                  withSpinner(DT::dataTableOutput("table1f"))
           ),
           column(width = 4,
                  withSpinner(uiOutput("aatresult"))
           )
         )
      ),
      tabPanel("2 - Manual Matching of Subjects",
         br(),
         fluidRow(
            column(width = 4, 
                   br(),
                   uiOutput("step1_msg"),
                   uiOutput("choose_string")
            ),
            column(width = 8,
                   uiOutput("topcategories")
            )
          ),
          fluidRow(
            column(width = 8,
                   DT::dataTableOutput("table1")
            ),
            column(width = 4,
                   uiOutput("table2")
              )
          )
      ),
      tabPanel("3 - Download Results",
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
      ),
      #Help----
      tabPanel("Help/About", 
           br(),
           fluidRow(
             column(width = 4,
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
                      heading = "Help",
                      status = "primary"
                    )
             ),
             column(width = 4,
                    shinyWidgets::panel(
                      HTML("<p>This app was made by the Digitization Program Office, OCIO.</p><p>The AAT is queried using their Linked Open Data SPARQL endpoint: <a href=\"http://vocab.getty.edu/queries\">http://vocab.getty.edu/queries</a></p>"),
                      heading = "About",
                      status = "primary"
                    )
             )
           ),
          HTML("<br><br><br><br><br><br><br><br>")
      )
  ),
  hr(),
  #footer
  HTML(paste0("<p><a href=\"http://dpo.si.edu\" target = _blank><img src=\"dpologo.jpg\"></a> | ", app_name, ", ver. ", app_ver, " | <a href=\"", github_link, "\" target = _blank>Source code</a></p>"))
)



#Server----
server <- function(input, output, session) {
  
  #Logging
  dir.create('logs', showWarnings = FALSE)
  flog.logger("getty", INFO, appender=appender.file(logfile))
  
  
  #main----
  output$main <- renderUI({
    shinyWidgets::panel(
      p("Match Getty AAT is a prototype system that matches terms in a file to the ", tags$strong("Getty Art & Architecture Thesaurus"), "using their Linked Open Data portal."),
      p("The app tries to find the best match by using a set of keywords included with each row, when available, to try to disambiguate the usage. For terms where many matches are found, the app allows the user to select the best one. Once the process is completed, the results file can be downloaded for further processing or importing to the CIS or other database."),
      heading = "Welcome",
      status = "primary"
    )
  })
  
  
  #inputfile----
  output$inputfile <- renderUI({
    shinyWidgets::panel(
      uiOutput("csv_info"),
      heading = "Input File",
      status = "success"
    )
  })
  
  
  #loadcsv ----
  output$loadcsv <- renderUI({
    if (is.null(input$csvinput)){
      tagList(
        fileInput("csvinput", "Upload an Input File",
            multiple = FALSE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv",
                       "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                       ".xlsx"), 
            width = "580px")#,
        #uiOutput("csv_info")
      )
    }
  })
  
  
  #csv_info----
  output$csv_info <- renderUI({
    if (is.null(input$csvinput)){
      HTML("<p>To use this app, upload a CSV or Excel file.
          <ul>
            <li>If the input file is a CSV file (.csv), the file must be comma-separated</li>
            <li>If the input file is an Excel file (.xlsx), only the first sheet is used</li>
          </ul>
          <p>The input file must be encoded using UTF-8 and have at least these 2 columns:</p>
          <ul>
             <li><samp>id</samp> - unique ID for the row</li>
             <li><samp>term</samp> - term to match to the AAT</li>
          </ul>
          <p>Two columns are optional:</p>
          <ul>
             <li><samp>keywords</samp> - words or phrases (separated by pipes: <samp>|</samp>) to filter possible term matches</li>
             <li><samp>linked_aat_term</samp> - AAT term from previous efforts</li>
          </ul>
          <p>Any other columns in the input file will be ignored but returned in the results file.</p>")
    }
  })
  
  
  #choose_string----
  output$choose_string <- renderUI({
    output$step1_msg <- renderUI({
      HTML("<br>
          <div class=\"alert alert-info\" role=\"alert\">Upload a file in Step 1 to run the matching system.</div>
          <br><br><br><br><br><br><br><br><br><br><br><br>")
    })

    req(input$csvinput)
    
    output$step1_msg <- renderUI({HTML("&nbsp;")})
    
    csvinput_db <- dbConnect(RSQLite::SQLite(), csv_database)
    csvinput1 <- dbGetQuery(csvinput_db, "SELECT term, id from csv WHERE aat_id IS NULL")
    dbDisconnect(csvinput_db)
    
    selectInput("row", "Select row to find a match in the AAT:", as.list(paste0(csvinput1$term, " (", csvinput1$id, ")")), width = "100%", multiple = FALSE, selectize = FALSE)
  })
  
  
  #aattop----
  output$aattop <- renderUI({
    getty_url <- "http://vocab.getty.edu/sparql.json?query="
    getty_top <- "select * {?f a gvp:Facet; skos:inScheme aat: ; gvp:prefLabelGVP/xl:literalForm ?l}"
    getty_query <- URLencode(getty_top, reserved = FALSE)
    json_top <- fromJSON(paste0(getty_url, getty_query))
    
    categories <- c("[All]", json_top$results$bindings$l$value)
    categories <- stringr::str_replace_all(categories, ' Facet', '')
    ids <- c("all", json_top$results$bindings$f$value)
    ids <- stringr::str_replace_all(ids, 'http://vocab.getty.edu/aat/', '')
    
    top_categories <- cbind(categories, ids)
    
    radioGroupButtons("topcats", "Search in this top AAT category:",
                      choiceNames = categories,
                      choiceValues = ids,
                      direction = "horizontal",
                      individual = TRUE,
                      status = "primary",
                      selected = "all")
  })
  
  
  #aatsub----
  output$aatsub <- renderUI({
    req(input$topcats)
    if (input$topcats != "all"){
      #Sub categories
      getty_url <- "http://vocab.getty.edu/sparql.json?query="
      getty_q <- "select * {?x gvp:broader aat:%s; skos:inScheme aat: ; gvp:prefLabelGVP/xl:literalForm ?l}"
      getty_q <- sprintf(getty_q, input$topcats)
      
      getty_query <- URLencode(getty_q, reserved = FALSE)
      json_top <- fromJSON(paste0(getty_url, getty_query))
      
      subcategories <- json_top$results$bindings$l$value
      subcategories <- stringr::str_replace_all(subcategories, 'hierarchy name', '')
      subcategories <- stringr::str_replace_all(subcategories, '[(]', '')
      subcategories <- stringr::str_replace_all(subcategories, '[)]', '')
      subcategories <- stringr::str_trim(subcategories)
      
      subcats_ids <- json_top$results$bindings$x$value
      subcats_ids <- stringr::str_replace_all(subcats_ids, 'http://vocab.getty.edu/aat/', '')
      
      sub_categories <- cbind(subcategories, subcats_ids)
      
      radioGroupButtons("subcats", "Search in this subcategory:",
                        choiceNames = subcategories,
                        choiceValues = subcats_ids,
                        direction = "horizontal",
                        individual = TRUE,
                        status = "default", 
                        selected = NA, 
                        size = "sm")
    }
  })
  
  
  #topcategories----
  output$topcategories <- renderUI({
    req(input$csvinput)
    tagList(
      br(),
      HTML("<div class=\"panel panel-primary\"> <div class=\"panel-heading\"> <h3 class=\"panel-title\">Filter results by AAT Top-level Subjects</h3> </div> <div class=\"panel-body\">"),
      uiOutput("aattop"),
      hr(),
      uiOutput("aatsub"),
      HTML("</div></div>")
    )
  })
  
  
  #table1----
  output$table1 <- DT::renderDataTable({
    
    req(input$csvinput)
    req(input$row)
    req(input$topcats)
    
    this_row_id <- base::strsplit(input$row, " [(]")[[1]]
    this_row_id <- this_row_id[length(this_row_id)]
    this_row_id <- base::strsplit(this_row_id, "[)]")[[1]][1]

    csvinput_db <- dbConnect(RSQLite::SQLite(), csv_database)
    this_query <- paste0("SELECT * FROM csv WHERE id = '", this_row_id, "'")
    flog.info(paste0("this_query: ", this_query), name = "locations")
    csvinput_db <- dbConnect(RSQLite::SQLite(), csv_database)
    this_row <- dbGetQuery(csvinput_db, this_query)
    dbDisconnect(csvinput_db)
    
    getty_url <- "http://vocab.getty.edu/sparql.json?query="
    getty_query <- "select ?Subject ?Term ?Parents ?ScopeNote {
            	          ?Subject a skos:Concept; luc:term \"%s\"; 
            	          gvp:broaderExtended aat:%s;
            	          skos:inScheme aat: ; 
            	          gvp:prefLabelGVP [xl:literalForm ?Term]. 
            	          optional {?Subject gvp:parentString ?Parents}
            	          optional {?Subject skos:scopeNote [dct:language gvp_lang:en; rdf:value ?ScopeNote]}
                      }"

    if (length(input$subcats) == 1){
      getty_query <- sprintf(getty_query, this_row$term, input$subcats)
    }else{
      if (input$topcats == "all"){
        getty_query <- sprintf(getty_query, this_row$term, "")
        getty_query <- stringr::str_replace(getty_query, "gvp:broaderExtended aat:;", "")
      }else{
        getty_query <- sprintf(getty_query, this_row$term, input$topcats)
      }
    }
    
    flog.info(paste0("Item query: ", getty_query), name = "getty")
    
    getty_query <- URLencode(getty_query, reserved = FALSE)
    json <- fromJSON(paste0(getty_url, getty_query))
    
    if (length(json$results$bindings$Subject$value) > 0){
      #Get the parent strings, remove pipes
      parents_strings <- str_replace_all(string = json$results$bindings$Parents$value, pattern = ">", replacement = "")
      parents_strings <- str_replace_all(string = parents_strings, pattern = "<", replacement = "|")
      parents_strings <- str_replace_all(string = parents_strings, pattern = ", ", replacement = "|")
      parents_strings <- str_replace_all(string = parents_strings, pattern = "[|][|]", replacement = "|")
      parents_strings <- str_replace_all(string = parents_strings, pattern = "[||]", replacement = "|")
      parents_strings <- str_replace_all(string = parents_strings, pattern = "[ ][)]", replacement = ")")
      parents_strings <- str_replace_all(string = parents_strings, pattern = "[|]", replacement = "|")
      
      for (p in 1:length(parents_strings)){
        parent_string <- parents_strings[p]
        parent_string_split <- stringr::str_split(parent_string, '[|]')[[1]]
        parent_string_split_ordered <- rev(parent_string_split)
        p_string <- parent_string_split_ordered
        p_string <- p_string[p_string != ""]
        for (s in 2:length(p_string)){
          p_string[s] <- paste0(paste0(rep('.', s - 1), collapse = ""), " ", p_string[s], collapse = "")
        }
        parents_strings[p] <- paste(p_string, collapse = "<br>")
      }
      
      results <<- data.frame(id = str_replace(string = json$results$bindings$Subject$value, pattern = "http://vocab.getty.edu/aat/", replacement = ""), term = json$results$bindings$Term$value, parents = parents_strings, note = json$results$bindings$ScopeNote$value)
      
      results_table <- dplyr::select(results, -1)
    }else{
      results <<- data.frame(id = NA, term = NA, parents = NA, note = NA)
      results_table <- dplyr::select(results, -1)
    }
    
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
                  caption = "Select a subject to match") %>% formatStyle("parents", "white-space" = "nowrap") %>% formatStyle("term", "white-space" = "nowrap")
  })
  
  
  #table2 ----
  output$table2 <- renderUI({
    req(input$csvinput)
    req(input$row)
    req(input$table1_rows_selected)
    
    res <- results[input$table1_rows_selected, ]
    
    AAT_url <- paste0("http://vocab.getty.edu/page/aat/", res$id)
    
    this_row_id <- base::strsplit(input$row, " [(]")[[1]]
    this_row_id <- this_row_id[length(this_row_id)]
    this_row_id <- base::strsplit(this_row_id, "[)]")[[1]][1]
    
    this_row_term <- base::strsplit(input$row, " [(]")[[1]]
    this_row_term <- this_row_term[1]
    
    output$insert_msg <- renderUI({HTML("&nbsp;")})
    
    csvinput_db <- dbConnect(RSQLite::SQLite(), csv_database)
    row_count <- dbGetQuery(csvinput_db, paste0("SELECT count(*) AS no_rows FROM csv WHERE aat_id IS NULL AND term = '", this_row_term, "'"))
    dbDisconnect(csvinput_db)
    
    tagList(
      HTML("<div class=\"panel panel-success\"> <div class=\"panel-heading\"> <h3 class=\"panel-title\">Result selected</h3> </div> <div class=\"panel-body\">"),
      HTML(paste0("<dl class=\"dl-horizontal\"><dt>Object ID</dt><dd>", this_row_id, "</dd><dt>Term</dt><dd>", res$term, "</dd><dt>AAT ID</dt><dd>", res$id)),
      actionLink("showaat", "[AAT page for this term]"),
      HTML("</dd></dl></p>"),
      # Save button
      actionButton("saverow", paste0("Save this AAT term for item ID ", this_row_id), class = "btn btn-primary", icon = icon("ok", lib = "glyphicon")),
      br(),
      br(),
      if (row_count$no_rows > 1){actionButton("saverowall", paste0("Save this AAT term for the ", row_count$no_rows, " rows with term '", this_row_term, "'"), class = "btn btn-primary", icon = icon("ok", lib = "glyphicon"))},
      uiOutput("insert_msg"),
      HTML("</div></div>")
    )
  })
  
  
  #showaat----
  observeEvent(input$showaat, {
    req(input$csvinput)
    req(input$row)
    req(input$table1_rows_selected)
    
    res <- results[input$table1_rows_selected, ]
    
    AAT_url <- paste0("http://vocab.getty.edu/page/aat/", res$id)
    
    showModal(modalDialog(
      title = "AAT Term Info",
      tags$iframe(src=AAT_url, height=650, width=860),
      easyClose = TRUE,
      footer = modalButton("Close", icon = icon("close")),
      size = "l")
    )
  })
  
 
  #saverow, observe----
  observeEvent(input$saverow, {
    req(input$csvinput)
    req(input$row)
    req(input$table1_rows_selected)
    
    this_row_id <- base::strsplit(input$row, " [(]")[[1]]
    this_row_id <- this_row_id[length(this_row_id)]
    this_row_id <- base::strsplit(this_row_id, "[)]")[[1]][1]
    
    selected_row <- results[input$table1_rows_selected, ]
    
    csvinput_db <- dbConnect(RSQLite::SQLite(), csv_database)
    query <- paste0("UPDATE csv SET aat_id = 'aat:", selected_row[1], "', aat_term = '", selected_row[2], "', aat_note = '", selected_row[3], "' WHERE id = '", this_row_id, "'")
    flog.info(paste0("update query: ", query), name = "getty")
    n <- dbExecute(csvinput_db, query)
    dbDisconnect(csvinput_db)
    output$insert_msg <- renderUI({
      HTML("<br><div class=\"alert alert-success\" role=\"alert\">Term saved</div>")
    })
  })
  
  
  #saverowall, observe----
  observeEvent(input$saverowall, {
    req(input$csvinput)
    req(input$row)
    req(input$table1_rows_selected)
    
    this_row_term <- base::strsplit(input$row, " [(]")[[1]]
    this_row_term <- this_row_term[1]
    
    selected_row <- results[input$table1_rows_selected, ]
    
    csvinput_db <- dbConnect(RSQLite::SQLite(), csv_database)
    query <- paste0("UPDATE csv SET aat_id = 'aat:", selected_row[1], "', aat_term = '", selected_row[2], "', aat_note = '", selected_row[3], "' WHERE term = '", this_row_term, "' AND aat_id IS NULL")
    flog.info(paste0("update query: ", query), name = "getty")
    n <- dbExecute(csvinput_db, query)
    dbDisconnect(csvinput_db)
    output$insert_msg <- renderUI({
      HTML("<br><div class=\"alert alert-success\" role=\"alert\">Term saved</div>")
    })
  })
  
  
  #downloadcsv1----
  output$downloadcsv1 <- downloadHandler(
    #Downloadable csv of results
    filename = function() {
      paste("results_aat_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      csvinput_db <- dbConnect(RSQLite::SQLite(), csv_database)
      data <- dbGetQuery(csvinput_db, "SELECT * FROM csv")
      dbDisconnect(csvinput_db)
      
      #Drop unused columns
      if (keywords_field == FALSE){
        data <- dplyr::select(data, -keywords)
      }
      
      if (linked_field == FALSE){
        data <- dplyr::select(data, -linked_aat_term)
      }
      
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  
  #downloadcsv2----
  output$downloadcsv2 <- downloadHandler(
    filename = function(){paste0("results_aat_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")},
    
    content = function(file){
      csvinput_db <- dbConnect(RSQLite::SQLite(), csv_database)
      data <- dbGetQuery(csvinput_db, "SELECT * FROM csv")
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
      HTML("<br><div class=\"alert alert-info\" role=\"alert\">Upload a file in Step 1 to run the matching system.</div><br><br><br><br><br><br><br><br><br><br><br><br>")
    })
    
    req(input$csvinput)
    
    output$step1_msg2 <- renderUI({HTML("&nbsp;")})
    
    shinyWidgets::panel(
      HTML("<p>Download the results as a Comma Separated Values file (.csv) or an Excel file (.xlsx).</p><p>The results file contains the same columns as the input file, untouched, with three additional columns:</p>"),
      HTML("<ul><li>aat_id</li><li>aat_term</li><li>aat_note</li></ul>"),
      br(),
      HTML("<div class=\"btn-toolbar\">"),
      downloadButton("downloadcsv1", "CSV (.csv)", class = "btn-success"),
      downloadButton("downloadcsv2", "Excel (.xlsx)", class = "btn-primary"),
      HTML("</div>"),
      heading = "Download Results",
      status = "primary"
    )
  })
  
  
  #table1f----
  output$table1f <- DT::renderDataTable({
    req(input$csvinput)
    
    #Read Upload----
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
    flog.info(paste0("this_query: ", this_query), name = "locations")
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
    
    this_query <- paste0("SELECT term, linked_aat_term, keywords FROM csv GROUP BY term, linked_aat_term, keywords")
    flog.info(paste0("this_query: ", this_query), name = "locations")
    all_rows <- dbGetQuery(csvinput_db, this_query)
    
    progress_val <- 0.01
    progress0 <- shiny::Progress$new()
    progress0$set(message = "Initializing. Please wait...", value = progress_val)
    on.exit(progress0$close())
    
    no_rows <- dim(all_rows)[1]
    
    # Calculate the number of cores
    no_cores <- detectCores() - 1
    # Initiate cluster
    cl <- makeCluster(no_cores)

    #Export data to cluster
    clusterExport(cl=cl, varlist=c("all_rows", "logfile"), envir=environment())

    #Divide into steps
    step_grouping <- no_cores * 2
    steps <- ceiling(dim(all_rows)[1] / step_grouping)

    progress_steps <- round(((0.9) / steps), 4)
    progress_val <- 0.1
    progress0$set(value = progress_val, message = "Querying AAT...")

    results <- list()

    #Run each batch, let the user know of the progress
    for (s in seq(1, steps)){
      to_row <- s * step_grouping
      from_row <- to_row - step_grouping

      if (from_row == 0){
        from_row <- 1
      }
      
      res <- parLapply(cl, seq(from_row, to_row), find_aat)
      progress_val <- (s * progress_steps) + 0.1
      progress0$set(value = progress_val, message = paste0("Querying AAT (", round(((s/steps) * 100), 2), "% completed)"))
      results <- c(results, res)
    }

    #stop cluster
    stopCluster(cl)
    progress0$set(message = "Done! Saving results...", value = 1)
    
    results <<- results
    
    progress0$close()
    progress1 <- shiny::Progress$new()
    on.exit(progress1$close())
    
    #Save the results to the database
    for (i in 1:length(results)){
      #print(paste0("row ", i, "\n"))
      if (!is.na(results[[i]]$aat_id)){
        #print(paste0("inserting row ", i, "\n"))
        this_query <- paste0("UPDATE csv SET aat_term = '", results[[i]]$aat_term, "', aat_id = '", results[[i]]$aat_id, "', aat_note = '", results[[i]]$aat_note, "' WHERE term = '", results[[i]]$term, "' AND keywords = '", results[[i]]$keywords, "' AND linked_aat_term = '", results[[i]]$linked_aat_term,"'")
        flog.info(paste0("this_query: ", this_query), name = "matches_aat")
        n <- dbSendQuery(csvinput_db, this_query)
        #db_commit(csvinput_db)
        dbClearResult(n)
      }
      progress1$set(message = "Saving results...", value = round(i/no_rows, 5))
    }
    
    progress1$set(message = "Done!", value = 1)

    #Get fresh version of table to display
    this_query <- paste0("SELECT * FROM csv")
    flog.info(paste0("this_query: ", this_query), name = "locations")
    resultsdf <<- dbGetQuery(csvinput_db, this_query)
    
    total_rows <- dim(resultsdf)[1]
    
    #How many matches?
    this_query <- paste0("SELECT COUNT(*) as no_matches FROM csv WHERE aat_id IS NOT NULL")
    flog.info(paste0("this_query: ", this_query), name = "locations")
    no_matches <- dbGetQuery(csvinput_db, this_query)$no_matches
    
    dbDisconnect(csvinput_db)
    
    results_table <- dplyr::select(resultsdf, -aat_note)
    
    DT::datatable(results_table, 
                  caption = paste0('Found ', no_matches, ' matches (of ', total_rows, ', ', round((no_matches/total_rows) * 100, 2), '%) in the Art & Architecture Thesaurus'), 
                  escape = FALSE, 
                  options = list(searching = TRUE, 
                                 ordering = TRUE, 
                                 pageLength = 10, 
                                 paging = TRUE), 
                  rownames = FALSE, 
                  selection = 'single')
  })

  
  #aatresult ----
  output$aatresult <- renderUI({
    #Display the AAT Subject and ID of the selected row
    req(input$table1f_rows_selected)
    
    res <- resultsdf[input$table1f_rows_selected, ]
    
    if (!is.na(res$aat_id)){
      AAT_url <- stringr::str_replace(res$aat_id, "aat:", "http://vocab.getty.edu/page/aat/")
      
      csvinput_db <- dbConnect(RSQLite::SQLite(), csv_database)
      
      this_query <- paste0("SELECT aat_note FROM csv WHERE aat_id = '", res$aat_id, "' LIMIT 1")
      flog.info(paste0("this_query: ", this_query), name = "locations")
      note <- dbGetQuery(csvinput_db, this_query)
      
      dbDisconnect(csvinput_db)
      
      AAT_term_notes <- note$aat_note

      tagList(
        HTML("<div class=\"panel panel-success\"> <div class=\"panel-heading\"> <h3 class=\"panel-title\">Result selected</h3> </div> <div class=\"panel-body\">"),
        HTML(paste0("<dl class=\"dl-horizontal\"><dt>Term</dt><dd>", res$aat_term, "</dd><dt>AAT ID</dt><dd>", res$aat_id, "<br>")),
        HTML(paste0("</dd><dt>Notes</dt><dd>", AAT_term_notes, "</dd></dl>")),
        HTML("</div></div>")
      )
    }
  })
}



#Run app----
shinyApp(ui = ui, server = server, onStart = function() {
  cat("Loading\n")
  #Cleanup on closing
  onStop(function() {
    cat("Closing\n")
    try(dbDisconnect(csvinput_db), silent = TRUE)
    try(unlink(csv_database), silent = TRUE)
  })
})
 