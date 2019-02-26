
find_aat <- function(row){
  #Query the AAT SPARQL API
  library(dplyr)
  library(stringr)
  library(jsonlite)
  library(futile.logger)

  flog.logger("parallel", INFO, appender=appender.file(logfile))
  
  getty_url <- "http://vocab.getty.edu/sparql.json?query="
  getty_query <- "select ?Subject ?Term {
                    ?Subject rdfs:label \"%s\"@en;
                    gvp:prefLabelGVP [xl:literalForm ?Term]}"
  
  #Remove single quotes
  getty_query <- sprintf(getty_query, stringr::str_replace_all(data[row,]$term, "'", ""))
  
  flog.info(paste0("Item query: ", getty_query), name = "getty")
  
  #URLEncode the query
  getty_query <- URLencode(getty_query, reserved = FALSE)
  json <- fromJSON(paste0(getty_url, getty_query))
  
  results <- as.data.frame(cbind(json$results$bindings$Subject$value, json$results$bindings$Term$value), stringsAsFactors = FALSE)
  
  if (dim(results)[1] == 1){
    #Found a single result, save it
    names(results) <- c("subject", "term")
    aat_term <- results$term
    aat_term <- stringr::str_replace(aat_term, "'", "''")
    aat_id <- paste0("aat:", stringr::str_replace(results$subject, "http://vocab.getty.edu/aat/", ""))
    
    return(list("aat_term" = aat_term, "aat_id" = aat_id, "term" = stringr::str_replace(data[row,]$term, "'", "''"), "filter" = data[row,]$filter))
  }else{
    #Found more than one, try full text
    getty_query <- "select ?Subject ?Term ?Parents ?ScopeNote {
            	          ?Subject a skos:Concept; luc:term \"%s\"; 
            	          skos:inScheme aat: ; 
            	          gvp:prefLabelGVP [xl:literalForm ?Term]. 
            	          optional {?Subject gvp:parentString ?Parents}
            	          optional {?Subject skos:scopeNote [dct:language gvp_lang:en; rdf:value ?ScopeNote]}
                      }"
    
    #Format the term for searching
    string_to_find <- stringr::str_replace_all(data[row,]$term, "'", "")
    string_to_find <- stringr::str_replace_all(string_to_find, ",", "")
    string_to_find <- stringr::str_replace_all(string_to_find, ";", "")
    string_to_find <- stringr::str_split(string_to_find, " ")[[1]]
    #Add wildcard to each word
    string_to_find <- paste0(string_to_find, '*')
    #Join words with AND
    string_to_find <- paste0(string_to_find, collapse = " AND ")
    
    getty_query <- sprintf(getty_query, string_to_find)
    
    flog.info(paste0("Item query: ", getty_query), name = "getty")
    
    getty_query <- URLencode(getty_query, reserved = FALSE)
    json <- fromJSON(paste0(getty_url, getty_query))
    
    df_results <- as.data.frame(cbind(json$results$bindings$Subject$value, json$results$bindings$Term$value, json$results$bindings$Parents$value, json$results$bindings$ScopeNote$value), stringsAsFactors = FALSE)
    
    if (dim(df_results)[1] > 0){
      names(df_results) <- c("subject", "term", "parents", "notes")
      
      if (data[row,]$filter != "" && dim(results)[1] == 1){
        results <- df_results %>% dplyr::filter(stringr::str_detect(notes, data[row,]$filter))
      }else{
        results <- df_results
      }
      
      if (dim(results)[1] == 1){
        aat_term <- results$term
        aat_term <- stringr::str_replace(aat_term, "'", "''")
        aat_id <- paste0("aat:", stringr::str_replace(results$subject, "http://vocab.getty.edu/aat/", ""))
        
        return(list("aat_term" = aat_term, "aat_id" = aat_id, "term" = stringr::str_replace(data[row,]$term, "'", "''"), "filter" = data[row,]$filter))
      }
    }
  }
  return(list("aat_term" = NA, "aat_id" = NA, "term" = stringr::str_replace(data[row,]$term, "'", "''"), "filter" = data[row,]$filter))
}

#Save the uploaded CSV into an SQLite database
csv_to_sqlite <- function(csv_database, csvinput){
  csvinput_db <- dbConnect(RSQLite::SQLite(), csv_database)
  n <- dbExecute(csvinput_db, 'PRAGMA encoding="UTF-8";')
  dbWriteTable(csvinput_db, "csv", csvinput, overwrite = TRUE)
  n <- dbExecute(csvinput_db, "ALTER TABLE csv ADD COLUMN aat_term TEXT;")
  n <- dbExecute(csvinput_db, "ALTER TABLE csv ADD COLUMN aat_id TEXT;")
  n <- dbExecute(csvinput_db, "CREATE INDEX id_index ON csv(id);")
  dbDisconnect(csvinput_db)
}
