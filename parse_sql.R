require(stringr)
require(readr)
#' cleans and separates multiple statements sql files into vectors of statements,
#' extreme basic use. WILL STILL FAIL in may scenarios, primary revolving around
#' comments.  Full PLSQL parsing it outside of the scope of this function.
#' @param sql.path A file path to an sql file to be parsed
#' @param params A list of parameters to replace in the sql code. Parameters
#' need to be contained by brackets in the sql file.
#' @return A vector of individual sql statements.
#' @examples 
#' sep.sql.stmts("path/to/file.sql", list(schema = "myschema"))
sep.sql.stmts <- function(sql.path, params = list()){
  raw.sql <- read_file(sql.path)
  # replaces items in the sql designated as parameters by an enclosing {}
  param.sql <-  raw.sql
  if (length(params) > 0){
    for (i in 1:length(params)){
      param.sql <- param.sql %>%
        str_replace_all(coll(paste0("{", names(params[i]), "}")), params[[i]])
    }
  }
    
  cleaned.sql <- param.sql %>%
    # simplifies line endings for platform independence / future regex 
    str_replace_all(pattern = coll("\r\n"), replacement = "\n") %>%
    # remove all demarked /*  */ sql comments 
    str_replace_all(pattern = regex("/\\*.*?\\*/", dotall = TRUE), 
                    replacement = " ") %>% 
    # remove all demarked -- comments 
    str_replace_all(pattern = regex('^--.*$', multiline = TRUE), 
                    replacement = " ") %>% 
    # remove any line break, tab, etc.
    # str_replace_all(pattern = '[\r\n\t\f\v]', replacement = " ") %>%  
    # remove extra whitespace 
    str_replace_all(pattern = ' +', replacement = ' ') %>%
    str_replace_all(pattern = '(?:\\s*\n\\s*)+', replacement = '\n')
    
  
  sql.stmts <- (cleaned.sql %>% str_split(";\\s*\n"))[[1]]
  
  # need to rejoin certain statements in the case of things like functions
  rejoined.stmts <- NULL
  current.stmt <-  NULL
  concat <-  FALSE
  for (stmt in sql.stmts){
    # cat(stmt)
    if(str_detect(stmt, pattern = "\\$[^\\$]*\\$")){
      concat <- !concat
      }
    current.stmt <- append(current.stmt, stmt)
    if (concat == FALSE){
      rejoined.stmts <-  append(rejoined.stmts, 
                                paste0(paste(current.stmt, collapse = ";\n"), 
                                       ";\n"))
      current.stmt <-  NULL
      }
  }
  return(rejoined.stmts)
}
