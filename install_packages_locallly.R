.libPaths("C:/Users/lochmannm/Documents/R/library-4.6.1")
packagelist <- c(
#  "arrow", 
#  "dbplyr",
#  "zoo",
#  "data.table", # till here: not necessary
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
  "exactextractr",
  "healtiar"
)
install.packages(packagelist, lib="C:/Users/lochmannm/Documents/R/library-4.6.1")

