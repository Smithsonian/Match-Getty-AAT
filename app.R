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


#Settings----
app_name <- "Match Getty AAT"
app_ver <- "0.3.0"
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
      tabPanel("Batch Matching of Subjects",
         fluidRow(
           column(width = 4, 
                  br(),
                  uiOutput("loadcsvf"),
                  uiOutput("downloadDataf")
           ),
           column(width = 8,
                  br(),
                  withSpinner(uiOutput("aatresult"))
           )
         ),
         hr(),
         withSpinner(DT::dataTableOutput("table1f"))
      ),
      tabPanel("Select Subject - Full Text Search",
        fluidRow(
            column(width = 4, 
                   br(),
                   uiOutput("loadcsv"),
                   fluidRow(
                     column(width = 9, 
                            uiOutput("choose_string")
                     ),
                     column(width = 3, 
                            br(),
                            uiOutput("nextButton")
                    )
                  )
            ),
            column(width = 8,
                   uiOutput("topcategories")
            )
          ),
          hr(),
          fluidRow(
            column(width = 8,
                   DT::dataTableOutput("table1")
            ),
            column(width = 4,
                   uiOutput("table2"),
                   uiOutput("downloadData")
              )
          )
      ),
      #Help tab
      tabPanel("Help", 
           br(),
           fluidRow(
             column(width = 6,
                    HTML("<p>This app will take the string in the column
                          \"term\" and match it with the The Art & Architecture
                          Thesaurus using their Linked Open Data portal.</p>
                        <p>Matching methods:</p>
                         <ul>
                           <li>Batch Matching of Subjects: Automated match 
                              based on the term and keywords.</li>
                           <li>Select Subject - Full Text Search: Select the 
                              appropiate match by searching in term names, 
                              notes, and other fields.</li>
                         </ul>")
             )
           ),
           fluidRow(
             column(width = 6,
                    HTML("<div class=\"panel panel-primary\"> 
                            <div class=\"panel-heading\"> 
                              <h3 class=\"panel-title\">Batch Matching of Subjects</h3>
                            </div>
                            <div class=\"panel-body\">
                               Coming soon...
                            </div></div>")
             ),
             column(width = 6,
                    HTML("<div class=\"panel panel-primary\"> 
                            <div class=\"panel-heading\"> 
                              <h3 class=\"panel-title\">Select Subject - Full Text Search</h3>
                            </div>
                            <div class=\"panel-body\">
                               Coming soon...
                            </div></div>")
             )
           )
      )
  ),
  hr(),
  #footer
  HTML(paste0("<p><a href=\"http://dpo.si.edu\" target = _blank><img src=\"dpologo.jpg\"></a> | ", app_name, " ver. ", app_ver, " | <a href=\"", github_link, "\" target = _blank>Source code</a></p>"))
)



#Server----
server <- function(input, output, session) {
  
  #Logging
  dir.create('logs', showWarnings = FALSE)
  flog.logger("getty", INFO, appender=appender.file(logfile))
  
  
  #loadcsv ----
  output$loadcsv <- renderUI({
    if (is.null(input$csvinput)){
      tagList(
        p("Upload a csv file to match to AAT terms using a full text search."),
        fileInput("csvinput", "Select csv File",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        uiOutput("csv_info")
      )
    }
  })
  
  
  
  #choose_string----
  output$choose_string <- renderUI({
    req(input$csvinput)
    
    csvinput <- read.csv(input$csvinput$datapath, header = TRUE, stringsAsFactors = FALSE)    
    
    # Process any error messages
    if (class(csvinput) == "try-error"){
        flog.error(paste0("Error reading CSV: ", csvinput), name = "csv")
      }else{
        #CSV to sqlite
        csv_to_sqlite(csv_database, csvinput)
      }
    
    flog.info(paste0("term: ", csvinput$term), name = "getty")
    flog.info(paste0("id: ", csvinput$id), name = "getty")
    
    selectInput("row", "Select row to find a match in the AAT:", as.list(paste0(csvinput$term, " (", csvinput$id, ")")), width = "100%", multiple = FALSE, selectize = FALSE)
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
    
    DT::datatable(results_table, escape = FALSE, options = list(searching = TRUE, ordering = TRUE, pageLength = 15, paging = TRUE, language = list(zeroRecords = "No matches found")), rownames = FALSE, selection = 'single', caption = "Subject") %>% formatStyle("parents", "white-space" = "nowrap") %>% formatStyle("term", "white-space" = "nowrap")
  })
  
  
  
  #csv_info----
  output$csv_info <- renderUI({
    if (is.null(input$csvinput)){
      HTML("<div class=\"panel panel-warning\"> <div class=\"panel-heading\"> <h3 class=\"panel-title\">The csv file must be comma-separated, encoded using UTF-8, and have at least these 2 columns</h3> </div> <div class=\"panel-body\"> <ul>
           <li>id</li>
           <li>term</li>
           </ul></div></div>")
      }
    })
  
  
  
  #table2 ----
  output$table2 <- renderUI({
    req(input$csvinput)
    req(input$row)
    req(input$table1_rows_selected)
    
    res <- results[input$table1_rows_selected, ]
    
    AAT_url <- paste0("http://vocab.getty.edu/page/aat/", res$id)
    
    tagList(
      HTML("<div class=\"panel panel-success\"> <div class=\"panel-heading\"> <h3 class=\"panel-title\">Result selected</h3> </div> <div class=\"panel-body\">"),
      HTML(paste0("<p>Term: ", res$term, "<br>AAT ID: <a href=\"", AAT_url, "\" target = _blank title = \"AAT page for this term\">", res$id, "</a></p>")),
      # Save button
      actionButton("saverow", "Save this term", class = "btn btn-primary", icon = icon("ok", lib = "glyphicon")),
      uiOutput("insert_msg"),
      HTML("</div></div>")
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
    query <- paste0("UPDATE csv SET aat_id = 'aat:", selected_row[1], "', aat_term = '", selected_row[2], "' WHERE id = '", this_row_id, "'")
    flog.info(paste0("update query: ", query), name = "getty")
    n <- dbExecute(csvinput_db, query)
    dbDisconnect(csvinput_db)
    output$insert_msg <- renderUI({
      HTML("<br><div class=\"alert alert-success\" role=\"alert\">Term saved</div>")
    })
  })
  
  
  
  #aat_display ----
  output$aat_display <- renderUI({
    req(input$csvinput)
    req(input$row)
    req(input$table1_rows_selected)
    
    res <- results[input$table1_rows_selected, ]
    AAT_url <- paste0("http://vocab.getty.edu/page/aat/", res$id)

    tagList(
      HTML(paste0("<a href=\"", AAT_url, "\" target = _blank>AAT page for this term</a>"))
    )
  })
  
  
  
  #downloadcsv1----
  output$downloadcsv1 <- downloadHandler(
    #Downloadable csv of results
    filename = function() {
      paste("results_aat_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      csvinput_db <- dbConnect(RSQLite::SQLite(), csv_database)
      write.csv(dbGetQuery(csvinput_db, "SELECT * FROM csv"), file, row.names = FALSE)
      dbDisconnect(csvinput_db)
    }
  )
  
  
  
  #downloadData ----
  output$downloadData <- renderUI({
    req(input$csvinput)
    downloadButton("downloadcsv1", "Download results", class = "btn-primary btn-sm")  
  })
  
  
  
  #nextButton----
  output$nextButton <- renderUI({
    req(input$csvinput)
    actionButton("nextButton", 
                 label = "Next >", 
                 class = "btn btn-primary btn-sm")
  })
  
  
  
  #nextButton, observe----
  observeEvent(input$nextButton, {
    
    csvinput <- read.csv(input$csvinput$datapath, header = TRUE, stringsAsFactors = FALSE)    
    seloptions <- as.list(paste0(csvinput$term, " (", csvinput$id, ")"))
    
    current <- which(seloptions == input$row)
    if(current < length(seloptions)){
      updateSelectInput(session, "row",
                        choices = as.list(seloptions),
                        selected = seloptions[current + 1])
    }
    output$insert_msg <- renderUI({HTML("")})
  })
  
  
  
  #loadcsvf ----
  output$loadcsvf <- renderUI({
    if (is.null(input$csvinputf)){
      tagList(
        p("Upload a csv file to match to AAT terms using keywords to limit the results."),
        fileInput("csvinputf", "Select csv File",
                  multiple = FALSE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        uiOutput("csv_infof")
      )
    }
  })
  
  
  #csv_infof----
  output$csv_infof <- renderUI({
    if (is.null(input$csvinputf)){
      HTML("<div class=\"panel panel-warning\"> <div class=\"panel-heading\"> <h3 class=\"panel-title\">The csv file must be comma-separated, encoded using UTF-8, and have at least these 2 columns</h3> </div> <div class=\"panel-body\"> <ul>
           <li>id</li>
           <li>term</li>
           </ul>
          <p>Two columns are optional:
          <ul>
           <li>keywords</li>
           <li>linked_aat_term</li>
           </ul>
           </ul><p>Separate words and phrases in <samp>keywords</samp> with pipes (<samp>|</samp>).</p></div></div>")
    }
  })

  
  #downloadcsv1f----
  output$downloadcsv1f <- downloadHandler(
    #Downloadable csv of results
    filename = function() {
      paste("results_aat_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv", sep = "")
    },
    content = function(file) {
      csvinput_db <- dbConnect(RSQLite::SQLite(), csv_database)
      write.csv(dbGetQuery(csvinput_db, "SELECT * FROM csv"), file, row.names = FALSE)
      dbDisconnect(csvinput_db)
    }
  )
  
  
  #downloadDataf ----
  output$downloadDataf <- renderUI({
    req(input$csvinputf)
    tagList(
      HTML("<div class=\"panel panel-primary\"> <div class=\"panel-heading\"> <h3 class=\"panel-title\">Download Results</h3> </div> <div class=\"panel-body\">"),
      downloadButton("downloadcsv1f", "Download full table as CSV", class = "btn-primary")  ,
      HTML("</div></div>")
    )
  })
    
  
  #table1f----
  output$table1f <- DT::renderDataTable({
    req(input$csvinputf)
    
    csvinput <- read.csv(input$csvinputf$datapath, header = TRUE, stringsAsFactors = FALSE)
    
    # Process any error messages
    if (class(csvinput) == "try-error"){
      flog.error(paste0("Error reading CSV: ", csvinput), name = "csv")
      req(FALSE)
    }else{
      #CSV to sqlite
      csv_to_sqlite(csv_database, csvinput)
    }
    
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
      keywords_field <- FALSE
    }else{
      keywords_field <- TRUE
    }
    
    if ("linked_aat_term" %in% table_info == FALSE){
      this_query <- paste0("ALTER TABLE csv ADD COLUMN linked_aat_term TEXT;")
      flog.info(paste0("this_query: ", this_query), name = "locations")
      n <- dbSendQuery(csvinput_db, this_query)
      linked_field <- FALSE
    }else{
      linked_field <- TRUE
    }
    
    if ("aat_notes" %in% table_info == FALSE){
      this_query <- paste0("ALTER TABLE csv ADD COLUMN aat_notes TEXT;")
      flog.info(paste0("this_query: ", this_query), name = "locations")
      n <- dbSendQuery(csvinput_db, this_query)
      aatnotes_field <- FALSE
    }else{
      aatnotes_field <- TRUE
    }
    
    this_query <- paste0("SELECT term, linked_aat_term, keywords FROM csv GROUP BY term, linked_aat_term, keywords")
    flog.info(paste0("this_query: ", this_query), name = "locations")
    all_rows <- dbGetQuery(csvinput_db, this_query)
    
    progress_val <- 0.01
    progress0 <- shiny::Progress$new()
    progress0$set(message = "Finding matches. Please wait...", value = progress_val)
    on.exit(progress0$close())
    
    no_rows <- dim(all_rows)[1]
    
    # Calculate the number of cores
    no_cores <- detectCores() - 1
    # Initiate cluster
    cl <- makeCluster(no_cores)

    #all_rows <- all_rows

    #Export data to cluster
    clusterExport(cl=cl, varlist=c("all_rows", "logfile"), envir=environment())

    #Divide into steps
    step_grouping <- no_cores * 3
    steps <- ceiling(dim(all_rows)[1] / step_grouping)

    progress_steps <- round(((0.9) / steps), 4)
    progress_val <- 0.1
    progress0$set(value = progress_val, message = "Querying AAT, please wait...")

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
      #progress0$set(value = progress_val, message = paste0("Querying AAT (", round(((s/steps) * 100), 2), "% completed)"))
      results <- c(results, res)
    }

    #stop cluster
    stopCluster(cl)
    progress0$set(message = "Done! Loading results...", value = 1)
    
    ##Non parallel
    # results <- list()
    # 
    # #Run each row, let the user know of the progress
    # for (s in seq(1, no_rows)){
    #   
    #   res <- find_aat(s)
    #   progress_val <- round(s / no_rows, 5)
    #   print(progress_val)
    #   #progress0$set(value = progress_val, message = paste0("Querying AAT (", round((progress_val * 100), 2), "% completed)"))
    #   results <- c(results, res)
    # }
    #
    #progress0$set(message = "Done! Loading results...", value = 1)
    
    #Save the results to the database
    for (i in 1:no_rows){
      if (!is.na(results[[i]]$aat_id)){
        this_query <- paste0("UPDATE csv SET aat_term = '", results[[i]]$aat_term, "', aat_id = '", results[[i]]$aat_id, "', aat_notes = '", results[[i]]$aat_notes, "' WHERE term = '", results[[i]]$term, "' AND keywords = '", results[[i]]$keywords, "'")
        flog.info(paste0("this_query: ", this_query), name = "matches_aat")
        n <- dbSendQuery(csvinput_db, this_query)
        dbClearResult(n)
      }
    }
    
    if (keywords_field == FALSE){
      n <- dbSendQuery(csvinput_db, "ALTER TABLE csv DROP COLUMN keywords")
    }
    if (linked_field == FALSE){
      n <- dbSendQuery(csvinput_db, "ALTER TABLE csv DROP COLUMN linked_aat_term")
    }
    
    #Get fresh version of table to display
    this_query <- paste0("SELECT * FROM csv")
    flog.info(paste0("this_query: ", this_query), name = "locations")
    resultsdf <<- dbGetQuery(csvinput_db, this_query)
    
    dbDisconnect(csvinput_db)
    
    results_table <- dplyr::select(resultsdf, -aat_notes)
    
    DT::datatable(resultsdf, caption = 'Matches found in the Art & Architecture Thesaurus.', escape = FALSE, options = list(searching = TRUE, ordering = TRUE, pageLength = 15, paging = TRUE), rownames = FALSE, selection = 'single')
  })

  
  
  #aatresult ----
  output$aatresult <- renderUI({
    #Display the AAT Subject and ID of the selected row
    req(input$table1f_rows_selected)
    
    res <- resultsdf[input$table1f_rows_selected, ]
    
    if (!is.na(res$aat_id)){
      AAT_url <- stringr::str_replace(res$aat_id, "aat:", "http://vocab.getty.edu/page/aat/")
      
      AAT_term_notes <- aat_term_info(stringr::str_replace(res$aat_id, "aat:", ""))
      
      tagList(
        HTML("<div class=\"panel panel-success\"> <div class=\"panel-heading\"> <h3 class=\"panel-title\">Result selected</h3> </div> <div class=\"panel-body\">"),
        HTML(paste0("<dl class=\"dl-horizontal\"><dt>Term</dt><dd>", res$aat_term, "</dd><dt>AAT ID</dt><dd><a href=\"", AAT_url, "\" target = _blank title = \"AAT page for this term\">", res$aat_id, "</a></dd><dt>Notes</dt><dd>", AAT_term_notes, "</dd></dl>")),
        HTML("</div></div>")
      )
    }
  })

  
  
  #help1 ----
  output$help1 <- renderUI({
    HTML("<div class=\"panel panel-primary\"> <div class=\"panel-heading\"> <h3 class=\"panel-title\">How this app works</h3></div><div class=\"panel-body\">
             <p>This app will take the string in the column \"term\" and match it with the The Art & Architecture Thesaurus using their Linked Open Data portal.</p>
             <p>Matching methods:</p>
               <ul>
                 <li>Term name only: Search for matches only in the subject term names.</li>
                 <li>Term, notes, etc.: Search in term names, notes, and other fields.</li>
               </ul>
            </div></div>")
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
