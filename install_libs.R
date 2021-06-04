#!/usr/bin/env Rscript

files = c("create_db.R", "export.R", "import.R", "lpi_calc.R", "lpi_convert.R",
          "parse_sql.R")

# find which packages we are currently using.
all.libs <-  character()
for (f in files){
  src <- readChar(f, file.info(f)$size)
  lib.match <- regexpr(pattern = "(libraries\\s+=\\s+c\\([^\\)]+\\))", 
                     src, perl = T)
  if (lib.match != -1) {
    lib.txt <- substr(src, lib.match, 
                      lib.match + attr(lib.match, "match.length"))
    eval(parse(text = lib.txt))
    all.libs <-  c(all.libs, libraries)
    rm("libraries")
  } else {
    cat(paste0("Error: could not parse libraries in file ", f))
  }
}
sorted.libs <- sort(unique(all.libs))

# figure out which ones need installing
need.install <- character()
for (lib in sorted.libs){
  if(lib %in% rownames(installed.packages()) == FALSE) {
    need.install <- c(need.install, lib)
  }
}
already.installed <- setdiff(sorted.libs, need.install)
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
      install.packages(lib, repos = "https://cloud.r-project.org")
    }
  }
} else {
  cat("No libraries need to be installed.\n")
}

cat("Script finished.\n")

