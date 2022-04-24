options(tidyverse.quiet = TRUE)
targets::tar_option_set(format = 'qs')
targets::tar_option_set(packages = c( "dplyr", "ggplot2", "readr", "tidyr","data.table","progressr","R6","pins","rsample","tidytext","purrr","furrr","magrittr","stringr","dtplyr"))
source("R/global_variables.R")
