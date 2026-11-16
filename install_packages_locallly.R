#.libPaths("C:/Users/lochmannm/Documents/R/library-4.6.2"
path<-"C:/Users/User/R/library-4.6.2"
.libPaths(path)
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
 # "healtiar" # not always available for newest R version
)
install.packages(packagelist, lib=path)

#install.packages("remotes",lib = path) # Only if not already installed
remotes::install_github(repo = "SwissTPH/healthiar", build_vignettes = TRUE, dependencies = TRUE, lib = path)
## In case, you want to develop R-packages
install.packages(c("devtools", "roxygen2", "testthat", "knitr"),lib=path)
