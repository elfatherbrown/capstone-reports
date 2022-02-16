source("globals.R")

gen_clean_files <- function(){
  list.files(en_data_dir,"*.txt") %>%
    clean_files()
  }

gen_clean_files()
