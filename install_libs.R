#!/usr/bin/env Rscript

files = c("create_db.R", "export.R", "import.R", "lpi_calc.R", "lpi_convert.R")

# find which packages we are currently using.
all.libs <-  character()
for (f in files){
  # print(f)
  src <- readChar(f, file.info(f)$size)
  lib.match <- gregexpr(pattern = r"{(?ms)^#?\s?libraries\s+=\s+c\(([^\)]+)\)}", 
                     src, perl = T,)
  if (all(lib.match[[1]] != -1)) {
    lib.txt <- substr(src, attr(lib.match[[1]], "capture.start"), 
                      attr(lib.match[[1]], "capture.start") + 
                        attr(lib.match[[1]], "capture.length") - 1)
    lib.cleaned <- gsub('"|\\s+|#', "", lib.txt)
    lib.split <- strsplit(lib.cleaned, ",", fixed = TRUE)[[1]]
    all.libs <-  c(all.libs, lib.split)
  } else {
    cat(paste0("Error: could not parse libraries in file ", f, "\n"))
  }
}

# find which github packages we are currently using.
git.libs <-  data.frame(name = character(), location = character(),
                        stringsAsFactors = FALSE)
for (f in files){
  # print(f)
  src <- readChar(f, file.info(f)$size)
  # need to pass regex mode set 'multiline' and 'dotall' as part of the pattern
  # see https://www.regular-expressions.info/modifiers.html
  pattern = r"{(?ms)^#?\s?github_libraries\s*=\s*c\((.+?\))[#\s]*\)}"
  gitlib.match <- gregexpr(pattern = pattern, src, perl = T)
  if (all(gitlib.match[[1]] != -1)) {
    gitlib.txt <- substr(src, attr(gitlib.match[[1]], "capture.start"), 
                      attr(gitlib.match[[1]], "capture.start") + 
                        attr(gitlib.match[[1]], "capture.length") - 1)
    gitlib.cleaned <- gsub(r'{\s+|#}', "", gitlib.txt)
    gitlib.split <- strsplit(gitlib.cleaned, r"{,(?=\s*list)}", perl = T)[[1]]
    for (g in gitlib.split){
      i <- nrow(git.libs) + 1
      git.libs[i,] <-  eval(parse(text = g))
    }
  } 
}

sorted.libs <- sort(unique(all.libs))
sorted.git <- git.libs[!duplicated(git.libs), ]
libs.df <- sorted.git
for (lib in sorted.libs){
  i <- nrow(libs.df) + 1
  libs.df[i,] <- c(lib, NA_character_)
}

# figure out which ones need installing
need.install <- character()
already.installed <- character()
for (lib in libs.df$name){
  if(lib %in% rownames(installed.packages()) == FALSE) {
    need.install <- c(need.install, lib)
  } else {
    already.installed <- c(already.installed, lib)
  }
}

# check to make sure we don't need to install devtools
ck <- libs.df[which(libs.df$name %in% need.install & !is.na(libs.df$location)),]
if (nrow(ck) > 0){
  if("devtools" %in% rownames(installed.packages()) == FALSE){
    libs.df <- rbind(c("devtools", NA_character_), libs.df)
    need.install <- c("devtools", need.install)
  }
}

if (length(already.installed) > 0){
  cat("The required libraries are already installed:\n\n")
  cat(paste0(paste(already.installed, collapse = ", "), "\n\n"))
}

if (length(need.install > 0)){
  cat("The following libraries need to be installed: \n\n")
  cat(paste0(paste(need.install, collapse = ", "), "\n\n"))
  if (interactive()){
    x <- tolower(substr(readline(prompt="Continue (y/n)?: "), 1, 1))
  } else {
    cat("Continue (y/n)?: ")
    resp <- readLines("stdin", n=1)
    x <- tolower(substr(resp, 1, 1))
  }
  if (x == "y"){
    for (lib in need.install){
      location = (libs.df[which(libs.df$name == lib),])$location
      if (is.na(location)){
        install.packages(lib, repos = "https://cloud.r-project.org")
      } else {
        devtools::install_github(location)
      }
    }
  }
} else {
  cat("No libraries need to be installed.\n")
}

cat("Script finished.\n")

