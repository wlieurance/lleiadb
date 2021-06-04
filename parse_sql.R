libraries = c("crayon", "readr", "stringr")
for (lib in libraries){
  suppressMessages(library(lib, character.only = TRUE))
}

#' cleans and separates multiple statements sql files into vectors of statements,
#' extreme basic use. WILL STILL FAIL in may scenarios, primary revolving around
#' comments.  Full PLSQL parsing it outside of the scope of this function.
#' @param sql.path A file path to an sql file to be parsed
#' @param params A list of parameters to replace in the sql code. Parameters
#' need to be contained by brackets in the sql file.
#' @return A vector of individual sql statements.
#' @examples 
#' sep.sql.stmts("path/to/file.sql", list(schema = "myschema"))
parse.sql.simple <- function(sql.path, params = list()){
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


#' Parses character string multi-statement SQL one character at a time and 
#' assigns a state to that character pertaining to whether it is single-quoted,
#' dollar quoted, a quoted identifier, or a comment.
#'
#' @param sql.path A character string. A path to a file containing SQL to parse. 
#' @param params A named list of character strings. Contains parameters to 
#'   replace in the sql code. Parameters need to be contained by brackets {} 
#'   in the SQL file.
#' @return A list containing a vector of characters for each SQL statement and
#'   a list of logical vectors which contain state data for each character. 
get.state <- function(sql.path, params = list()){
  raw.sql <- read_file(sql.path)
  # replaces items in the sql designated as parameters by an enclosing {}
  param.sql.s <-  raw.sql
  if (length(params) > 0){
    for (i in 1:length(params)){
      param.sql <- param.sql %>%
        str_replace_all(coll(paste0("{", names(params[i]), "}")), params[[i]])
    }
  }
  param.sql <- strsplit(param.sql.s, "")[[1]]
  # char_no <- nchar(param.sql.s)
  param_len <- length(param.sql)
  if (param_len == 0){
    return(param.sql)
  }
  
  current <- character()
  current.v <- list()
  state.list <- list()
  state.v <- list()
  state <- c(
    lc = FALSE,  # line comment
    bc = FALSE,  # block comment
    dt1 = FALSE,  # first tag within the dollar quote
    dt2 = FALSE,  # second tag within the dollar quote
    dc = FALSE,  # string constant within the dollar quote
    sq = FALSE,  # single quoted string constant
    qi = FALSE  # quoted identifier
  )
  begin <-  TRUE
  start <-  0
  for (i in seq(1, param_len)){
    new.state <- state
    if (paste0(param.sql[i], param.sql[i+1]) == "--"){
      if (all(!new.state)){
        new.state['lc'] <- TRUE
        begin <- TRUE
      }
    }
    if (param.sql[i] == "\n"){
      if (new.state["lc"]){
        new.state["lc"] <- FALSE
        begin <- FALSE
      }
    }
    if (paste0(param.sql[i], param.sql[i+1]) == "/*"){
      if (all(!new.state)){
        new.state["bc"] <- TRUE
        begin <- TRUE
      }
    }
    if (paste0(param.sql[i-1], param.sql[i])== "*/"){
      if (new.state["bc"]){
        new.state["bc"] <- FALSE
        begin <- FALSE
      }
    }
    if (param.sql[i] == "$"){
      if (all(!new.state)){
        new.state["dt1"] <- TRUE 
        begin <- TRUE
      } else if (new.state["dt1"] && 
                 all(!new.state[!names(new.state) %in% c("dt1")])){
        new.state["dt1"] <- FALSE
        new.state["dc"] <- TRUE
        begin <- FALSE
      } else if (new.state["dc"] && 
                 all(!new.state[!names(new.state) %in% c("dc")])){
        begin <- TRUE
        new.state["dc"] <- FALSE
        new.state["dt2"] <- TRUE
      } else if (new.state["dt2"] && 
                 all(!new.state[!names(new.state) %in% c("dt2")])){
        new.state["dt2"] <- FALSE
        begin <- FALSE
      }
    }
    if (param.sql[i] == "'" && 
        paste0(param.sql[i], param.sql[i+1]) != "''" &&
        paste0(param.sql[i-1], param.sql[i]) != "''"){
      if (all(!new.state)){
        new.state["sq"] <-  TRUE
        begin <- TRUE
      } else if (new.state["sq"] 
                 && all(!new.state[!names(new.state) %in% c("sq")])){
        new.state["sq"] <- FALSE
        begin <- FALSE
      }
    }
    if (param.sql[i] == '"' && 
        paste0(param.sql[i], param.sql[i+1]) != '""' &&
        paste0(param.sql[i-1], param.sql[i]) != '""'){
      if (all(!new.state)){
        new.state["qi"] <- TRUE
        begin <- TRUE
      } else if (new.state["qi"] && 
                 all(!new.state[!names(new.state) %in% c("qi")])){
        new.state["qi"] <- FALSE
        begin <- FALSE
      }
    }
    
    if (begin){
      state.list[[i - start]] <- new.state
      # f <- parse.cat(param.sql[i], new.state)
    } else {
      state.list[[i - start]] <- state
      # f <- parse.cat(param.sql[i], state)
    }
    current[i - start] <- param.sql[i]
    # formatted[i - start] <- f
    state <- new.state
    
    if (param.sql[i] == ";" && all(!new.state)){
      current.v[[length(current.v) + 1]] <- current
      state.v[[length(state.v) + 1]] <- state.list
      current <- character()
      formatted <- character()
      state.list <- list()
      start <- i
    }
  }
  return(list(stmts = current.v, states = state.v))
}

#' Takes a character and a state and assigns a color and style to the character
#' using the crayon package.
#'
#' @param my.chars A character string contaiing the text to format.
#' @param my.state A named logical vector containing state information for 
#'   \code{my.chars}.
#'
#' @return A color/style formatted version of \code{my.chars}.
parse.cat <- function(my.chars, my.state){
  if (my.state["lc"]) {
    text <- green(my.chars)
  } else if (my.state["bc"]) {
    text <- yellow(my.chars)
  } else if (any(my.state[c("dt1", "dc", "dt2")])) {
    text <- blue(my.chars)  
  } else if (my.state["sq"]) {
    text <- red(my.chars)
  } else if (my.state["qi"]) {
    text <- cyan(my.chars)
  } else {
    text <- my.chars
  }
  if (any(my.state[c("dt1", "dt2")])) {
    text <- bgYellow(text)
  } 
  return(text)
}

#' A vectorized version of \code{parse.cat()}
parse.cat.v <- Vectorize(parse.cat)


#' Checks if text matches a PostgreSQL key word and returns a formated version
#' of it if it does.
#'
#' @param text A character string.
#' @param key.words A vector of character strings.
#'
#' @return A formatted version of \code{text}.
check.keywords <- function(text, key.words){
  if (str_to_upper(text) %in% key.words){
    return(underline(bold(text)))
  } else {
    return(text)
  }
}


#' Takes the character vector and state list and combines them into a formatted
#' SQL statement.   
#'
#' @param char.v A character string vector.
#' @param state.l A list of logical vectors. Must be the same length as 
#'   \code{char.v}.
#' @param key.words A chracter string vector.
#'
#' @return A formatted character string. Formatted with package \code{crayon}.
parse.combine <- function(char.v, state.l, key.words){
  total.words <- character()
  new.word <- character()
  begin <- FALSE
  for (i in seq(1, length(char.v))){
    char <- char.v[i]
    state <- state.l[[i]]
    if (all(!state) && grepl("[^\\s]", char, perl = T)){
      new.word <- paste0(new.word, char)
      if (!begin){begin <- TRUE}
    } else {
      if (begin){
        begin <- FALSE
        total.words <- paste0(total.words, check.keywords(new.word, key.words)) 
        new.word <- character()
      }
      total.words <- paste0(total.words, parse.cat(char, state))
    }
  }
  if (nchar(new.word) > 0){
    total.words <- paste0(total.words, check.keywords(new.word, key.words))
    new.word <- character()
  }
  return(total.words)
}

#' Reads in multi-statement SQL, separates it into individual 
#' statements, and optionally, generates a formatted output that differentiates
#' SQL key words, comments, quoted constants, and quoted identifiers. 
#'
#' @param sql.path A character string. A path to a file containing SQL to parse. 
#' @param params A named list of character strings. Contains parameters to 
#'   replace in the sql code. Parameters need to be contained by brackets {} 
#'   in the SQL file.
#' @param format Logical. A flag telling the function to create a color/style
#'   formatted version of the SQL for viewing.  This dramatically increases the 
#'   run time of the function. 
#' @param verbose Logical. A flag telling the function to print each parsed SQL
#'   statement, separated by a line of dashes.
#'
#' @return A list of two character vectors, one containing the unformatted SQL
#'   statements and one containing the formatted version (if \code{format}).
#' @export
parse.sql <-  function(sql.path, params = list(), format = FALSE, 
                            verbose = FALSE){
  formatted <- NULL
  parsed <- get.state(sql.path = sql.path, params = params)
  noformat <- sapply(parsed$stmts, paste, collapse = "")
  if (format){
    pgkw <- read_csv("./tabular/pg_keywords.csv", 
                     col_types = "ccll") 
    formatted <- mapply(parse.combine, char.v=parsed$stmts, 
                        state.l=parsed$states, 
                        MoreArgs=list(key.words=pgkw$key_word))
    
    # # older, quicker version way to create formatted, but does not detect
    # # key words.
    # formatted.v <- mapply(FUN = parse.cat.v, my.chars = parsed$stmts,
    #                       my.state = parsed$states)
    # formatted <- sapply(formatted.v, paste, collapse = "")
  }
  if (verbose){
    if (!format){
      for (i in seq(1:length(noformat))){
        cat(noformat[i])
        cat("\n----------------------------------------------\n")
      }
    } else {
      for (i in seq(1:length(formatted))){
        cat(formatted[i])
        cat("\n----------------------------------------------\n")
      }
    }
  }
  return(list(noformat = noformat, formatted = formatted))
}

