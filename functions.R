find_aat <- function(which_row){
  #Query the AAT SPARQL API
  library(dplyr)
  library(stringr)
  library(jsonlite)
  library(futile.logger)
  
  this_row <- all_rows[which_row,]
  
  flog.logger("parallel", INFO, appender=appender.file(logfile))
  getty_url <- "http://vocab.getty.edu/sparql.json?query="
  
  if (!is.na(this_row$linked_aat_term)){
    getty_query <- "select ?subj ?ScopeNote {?subj gvp:prefLabelGVP/xl:literalForm \"%s\"@en; skos:inScheme aat:; 
                        optional {?subj skos:scopeNote [dct:language gvp_lang:en; rdf:value ?ScopeNote]}
  }"
    
    #Remove single quotes
    getty_query <- sprintf(getty_query, stringr::str_replace_all(this_row$linked_aat_term, "'", ""))
    
    flog.info(paste0("Item query: ", getty_query), name = "getty")
    
    #URLEncode the query
    getty_query <- URLencode(getty_query, reserved = FALSE)
    
    json <- fromJSON(paste0(getty_url, getty_query))
    
    if (length(json$results$bindings) != 0){
      aat_id <- paste0("aat:", stringr::str_replace(json$results$bindings$subj$value, "http://vocab.getty.edu/aat/", ""))
      scopenote <- json$results$bindings$ScopeNote$value
      
      return(list("aat_term" = this_row$linked_aat_term, "aat_id" = aat_id, "term" = stringr::str_replace(this_row$term, "'", "''"), "keywords" = this_row$keywords, "linked_aat_term" = this_row$linked_aat_term, "aat_note" = scopenote))
    }
  }
  
  getty_query <- "select ?Subject ?Term ?ScopeNote {
                    ?Subject rdfs:label \"%s\"@en;
                    gvp:prefLabelGVP [xl:literalForm ?Term];
                    optional {?Subject skos:scopeNote [dct:language gvp_lang:en; rdf:value ?ScopeNote]}}"
  
  #Remove single quotes
  getty_query <- sprintf(getty_query, stringr::str_replace_all(this_row$term, "'", ""))
  
  flog.info(paste0("Item query: ", getty_query), name = "getty")
  
  #URLEncode the query
  getty_query <- URLencode(getty_query, reserved = FALSE)
  json <- fromJSON(paste0(getty_url, getty_query))
  
  results <- as.data.frame(cbind(json$results$bindings$Subject$value, json$results$bindings$Term$value, json$results$bindings$ScopeNote$value), stringsAsFactors = FALSE)
  
  if (dim(results)[1] == 1){
    #Found a single result, save it
    if (dim(results)[2] == 2){
      #ScopeNote is empty
      results <- as.data.frame(cbind(json$results$bindings$Subject$value, json$results$bindings$Term$value, ""), stringsAsFactors = FALSE)
    }
    names(results) <- c("subject", "term", "aat_note")
    aat_term <- results$term
    aat_term <- stringr::str_replace(aat_term, "'", "''")
    aat_id <- paste0("aat:", stringr::str_replace(results$subject, "http://vocab.getty.edu/aat/", ""))
    scopenote <- results$aat_note
    
    return(list("aat_term" = aat_term, "aat_id" = aat_id, "term" = stringr::str_replace(this_row$term, "'", "''"), "keywords" = this_row$keywords, "linked_aat_term" = NA, "aat_note" = scopenote))
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
    string_to_find <- stringr::str_replace_all(this_row$term, "'", "")
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
      names(df_results) <- c("subject", "term", "parents", "aat_note")
      
      if (!is.na(this_row$keywords) && dim(df_results)[1] != 1){
        results <- df_results %>% dplyr::filter(stringr::str_detect(aat_note, this_row$keywords))
      }else{
        results <- df_results
      }
      
      if (dim(results)[1] == 1){
        aat_term <- results$term
        aat_term <- stringr::str_replace(aat_term, "'", "''")
        aat_id <- paste0("aat:", stringr::str_replace(results$subject, "http://vocab.getty.edu/aat/", ""))
        scopenote <- results$aat_note
        
        return(list("aat_term" = aat_term, "aat_id" = aat_id, "term" = stringr::str_replace(this_row$term, "'", "''"), "keywords" = this_row$keywords, "linked_aat_term" = this_row$linked_aat_term, "aat_note" = scopenote))
      }
    }
  }
  return(list("aat_term" = NA, "aat_id" = NA, "term" = stringr::str_replace(this_row$term, "'", "''"), "keywords" = this_row$keywords, "linked_aat_term" = this_row$linked_aat_term, "aat_note" = NA))
}


aat_term_info <- function(aat_id){
  #Query the AAT SPARQL API
  library(dplyr)
  library(stringr)
  library(jsonlite)
  library(futile.logger)
  
  flog.logger("parallel", INFO, appender=appender.file(logfile))
  
  term_json <- paste0("http://vocab.getty.edu/aat/", aat_id, ".json")
  
  flog.info(paste0("term_json: ", term_json), name = "getty")
  
  json <- fromJSON(term_json)
  
  term_notes <- json$results$bindings$Object[json$results$bindings$Predicate$value == "http://www.w3.org/1999/02/22-rdf-syntax-ns#value",]
  
  if (dim(term_notes)[1] == 0){
    return(NA)
  }else if (dim(term_notes)[1] == 1){
    return(term_notes$value[1])
  }else{
    term_notes_en <- term_notes$value[term_notes$`xml:lang`=='en']
    if (term_notes_en!=""){
      return(term_notes_en)
    }else{
      return(paste0(term_notes$value, collapse = "<br>"))
    }
  }
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
