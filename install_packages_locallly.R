.libPaths("M:/R/library-4.6")
packagelist <- c(
  "arrow", 
  "dbplyr",
  "zoo",
  "data.table", # till here: not necessary
  "tidyverse", # from here: necessary for END2DALY
  "readxl",
  "purrr",
  "janitor",
  "rmarkdown",
  "flextable",
  "knitr",
  "remotes",
  "terra",
  "sf",
  "exactextractr"
)
install.packages(packagelist, lib="M:/R/library-4.6")
