
find_aat <- function(which_row){
  #Query the AAT SPARQL API
  library(dplyr)
  library(stringr)
  library(jsonlite)
  library(futile.logger)
  library(RPostgres)
  library(tokenizers)
  library(stopwords)
  
  this_row <- all_rows[which_row,]
  
  flog.logger("parallel", INFO, appender=appender.file(logfile))
  getty_url <- "http://vocab.getty.edu/sparql.json?query="
  
  flog.info(paste0("which_row: ", which_row), name = "parallel")
  
  if (!is.na(this_row$linked_aat_term)){
    getty_query <- "select 
                      ?subj ?ScopeNote 
                      {
                        ?subj gvp:prefLabelGVP/xl:literalForm \"%s\"@en; 
                        skos:inScheme aat:; 
                        optional {
                          ?subj skos:scopeNote [dct:language gvp_lang:en; rdf:value ?ScopeNote]
                        }
                      }"
    
    #Replace search string in query, also remove single quotes
    getty_query <- sprintf(getty_query, stringr::str_replace_all(this_row$linked_aat_term, "'", ""))
    flog.info(paste0("Item query: ", getty_query), name = "parallel")
    
    #URLEncode the query
    getty_query <- URLencode(getty_query, reserved = FALSE)
    
    error_check <- try(json <- fromJSON(paste0(getty_url, getty_query)), silent = TRUE)
    
    if (class(error_check) == "try-error"){
      flog.error(paste0("AAT query failed: (", this_row$rowid, ") ", paste0(getty_url, getty_query)), name = "parallel")
      return(list("csv_rowid" = this_row$rowid, "aat_term" = NA, "aat_id" = NA, "aat_note" = NA))
    }
    
    if (length(json$results$bindings) != 0){
      aat_id <- paste0("aat:", stringr::str_replace(json$results$bindings$subj$value, "http://vocab.getty.edu/aat/", ""))
      scopenote <- stringr::str_replace_all(json$results$bindings$ScopeNote$value, "'", "''")

      return(list("csv_rowid" = this_row$rowid, "aat_term" = this_row$linked_aat_term, "aat_id" = aat_id, "aat_note" = scopenote))
    }
  }
  
  getty_query <- "select 
                    ?Subject ?Term ?ScopeNote {
                        ?Subject rdfs:label \"%s\"@en;
                        skos:inScheme aat:; 
                        gvp:prefLabelGVP [xl:literalForm ?Term];
                        optional {
                            ?Subject skos:scopeNote [dct:language gvp_lang:en; rdf:value ?ScopeNote]
                          }
                      }"
  
  #Replace search string in query, also remove single quotes
  getty_query <- sprintf(getty_query, stringr::str_replace_all(this_row$term, "'", ""))
  
  flog.info(paste0("Item query: ", getty_query), name = "parallel")
  
  #URLEncode the query
  getty_query <- URLencode(getty_query, reserved = FALSE)
  
  error_check <- try(json <- fromJSON(paste0(getty_url, getty_query)), silent = TRUE)
  
  if (class(error_check) == "try-error"){
    flog.error(paste0("AAT query failed: (", this_row$rowid, ") ", paste0(getty_url, getty_query)), name = "parallel")
    return(list("csv_rowid" = this_row$rowid, "aat_term" = NA, "aat_id" = NA, "aat_note" = NA))
  }
  
  results <- as.data.frame(cbind(json$results$bindings$Subject$value, json$results$bindings$Term$value, json$results$bindings$ScopeNote$value), stringsAsFactors = FALSE)
  
  if (dim(results)[1] == 1){
    #Found a single result, save it
    if (dim(results)[2] == 2){
      #ScopeNote is empty
      results <- as.data.frame(cbind(json$results$bindings$Subject$value, json$results$bindings$Term$value, ""), stringsAsFactors = FALSE)
    }
    names(results) <- c("subject", "term", "aat_note")
    aat_term <- results$term
    aat_term <- stringr::str_replace_all(aat_term, "'", "''")
    aat_id <- paste0("aat:", stringr::str_replace(results$subject, "http://vocab.getty.edu/aat/", ""))
    scopenote <- stringr::str_replace_all(results$aat_note, "'", "''")
    
    return(list("csv_rowid" = this_row$rowid, "aat_term" = aat_term, "aat_id" = aat_id, "aat_note" = scopenote))
  }else{
    #Found more than one, try full text
    getty_query <- "select 
                        ?Subject ?Term ?Parents ?ScopeNote {
              	          ?Subject a skos:Concept; luc:term \"%s\"; 
              	          skos:inScheme aat: ; 
              	          gvp:prefLabelGVP [xl:literalForm ?Term]. 
              	          optional {
              	              ?Subject gvp:parentString ?Parents
              	              }
              	          optional {
              	              ?Subject skos:scopeNote [dct:language gvp_lang:en; rdf:value ?ScopeNote]
              	              }
                        }"
    
    #Format the term for searching
    string_to_find <- stringr::str_replace_all(this_row$term, "'", "")
    string_to_find <- stringr::str_replace_all(string_to_find, ",", "")
    string_to_find <- stringr::str_replace_all(string_to_find, ";", "")
    string_to_find <- stringr::str_split(string_to_find, " ")[[1]]
    string_formatted <- paste0(string_to_find, collapse = " AND ")
    
    #Parse keywords
    keywords_tokenized <- tokenizers::tokenize_words(this_row$keywords, stopwords = stopwords::stopwords("en"))[[1]]
    
    if (length(keywords_tokenized) == 1 && is.na(keywords_tokenized)){
      
      #No keywords
      #Replace search string in query
      getty_query_ready <- sprintf(getty_query, string_formatted)
      
      flog.info(paste0("Item query: ", getty_query_ready), name = "parallel")
      
      #URLencode the query
      getty_query_ready <- URLencode(getty_query_ready, reserved = FALSE)
      
      error_check <- try(json <- fromJSON(paste0(getty_url, getty_query_ready)), silent = TRUE)
      
      if (class(error_check) == "try-error"){
        flog.error(paste0("AAT query failed: (", this_row$rowid, ") ", paste0(getty_url, getty_query_ready)), name = "parallel")
        return(list("csv_rowid" = this_row$rowid, "aat_term" = NA, "aat_id" = NA, "aat_note" = NA))
      }
      
      df_results <- as.data.frame(cbind(json$results$bindings$Subject$value, json$results$bindings$Term$value, json$results$bindings$ScopeNote$value), stringsAsFactors = FALSE)
      
      results = data.frame(matrix(nrow = 0, ncol = 4, data = NA))
      
      if (dim(df_results)[1] > 0){
        names(df_results) <- c("subject", "term", "aat_note")
        
        for (r in seq(1, dim(df_results)[1])){
          results <- rbind(results, 
                           cbind(
                             this_row$rowid, 
                             stringr::str_replace_all(df_results$term[r], "'", "''"),
                             paste0("aat:", stringr::str_replace(df_results$subject[r], "http://vocab.getty.edu/aat/", "")),
                             stringr::str_replace_all(df_results$aat_note[r], "'", "''")
                           )
          )
        }
      }
    }else{
      for (t in seq(1, length(keywords_tokenized))){
        #Join words with AND
        string_to_query <- paste0(string_formatted, " AND ", keywords_tokenized[t])
        
        #Replace search string in query
        getty_query_ready <- sprintf(getty_query, string_to_query)
        
        flog.info(paste0("Item query: ", getty_query_ready), name = "parallel")
        
        #URLencode the query
        getty_query_ready <- URLencode(getty_query_ready, reserved = FALSE)
        
        error_check <- try(json <- fromJSON(paste0(getty_url, getty_query_ready)), silent = TRUE)
        
        if (class(error_check) == "try-error"){
          flog.error(paste0("AAT query failed: (", this_row$rowid, ") ", paste0(getty_url, getty_query_ready)), name = "parallel")
          return(list("csv_rowid" = this_row$rowid, "aat_term" = NA, "aat_id" = NA, "aat_note" = NA))
        }
        
        df_results <- as.data.frame(cbind(json$results$bindings$Subject$value, json$results$bindings$Term$value, json$results$bindings$Parents$value, json$results$bindings$ScopeNote$value), stringsAsFactors = FALSE)
        
        results = data.frame(matrix(nrow = 0, ncol = 4, data = NA))
        
        if (dim(df_results)[1] > 0){
          names(df_results) <- c("subject", "term", "parents", "aat_note")
          
          for (r in seq(1, dim(df_results)[1])){
            results <- rbind(results, 
                             cbind(
                               this_row$rowid, 
                               stringr::str_replace_all(df_results$term[r], "'", "''"),
                               paste0("aat:", stringr::str_replace(df_results$subject[r], "http://vocab.getty.edu/aat/", "")),
                               stringr::str_replace_all(df_results$aat_note[r], "'", "''")
                             )
            )
          }
        }else{
          return(list("csv_rowid" = this_row$rowid, "aat_term" = NA, "aat_id" = NA, "aat_note" = NA))
        }
      }
    }
    
    names(results) = c("csv_rowid", "aat_term", "aat_id", "aat_note")
    return(as.list(results))
  }
  return(list("csv_rowid" = this_row$rowid, "aat_term" = NA, "aat_id" = NA, "aat_note" = NA))
}


#Save the input file into a SQLite database
csv_to_sqlite <- function(csv_database, csvinput){
  csvinput_db <- dbConnect(RSQLite::SQLite(), csv_database)
  
  #UTF-8 encoding
  n <- dbExecute(csvinput_db, 'PRAGMA encoding="UTF-8";')
  
  #Write data to table "csv"
  dbWriteTable(csvinput_db, "csv", csvinput, overwrite = TRUE)
  
  #Index columns used for searching
  n <- dbExecute(csvinput_db, "CREATE INDEX id_index ON csv(id);")
  n <- dbExecute(csvinput_db, "CREATE INDEX id_term ON csv(term);")
  
  
  
  #Close file
  dbDisconnect(csvinput_db)
}


term_score <- function(term, keywords){
  
  getty_url <- "http://vocab.getty.edu/sparql.json?query="
  
  getty_query <- "select ?Subject {
            	          ?Subject a skos:Concept; luc:term \"%s\"; 
            	          skos:inScheme aat: ;
                      }"
  
  #break up the keyword text, removing stop words
  kwords <- tokenize_words(keywords, stopwords = stopwords::stopwords("en"), simplify = TRUE)

  #process each keyword
  for (w in seq(1, length(keywords))){
    #Join words with AND
    string_to_find <- paste0(term, " AND ", keywords[w])
    
    #Replace search string in query
    getty_query <- sprintf(getty_query, string_to_find)
    
    flog.info(paste0("Item query: ", getty_query), name = "getty")
    
    #URLencode the query
    getty_query <- URLencode(getty_query, reserved = FALSE)
    
    json <- fromJSON(paste0(getty_url, getty_query))
    
    #CONTINUE
  }
  
  
  
}
