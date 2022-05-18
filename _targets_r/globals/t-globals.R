package_list <-
  c(
    "dplyr",
    "ggplot2",
    "readr",
    "tidyr",
    "data.table",
    "progressr",
    "R6",
    "pins",
    "rsample",
    "tidytext",
    "purrr",
    "furrr",
    "magrittr",
    "stringr",
    "dtplyr",
    "futile.logger",
     "tarchetypes"
  )
# options(tidyverse.quiet = TRUE)
targets::tar_option_set(format = 'qs')
targets::tar_option_set(packages = package_list)
library(targets)
xfun::pkg_attach(package_list)
source("R/global_variables.R")
options(datatable.verbose = FALSE)

