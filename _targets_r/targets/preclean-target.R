source("R/preclean_functions.R")

  tar_target(preclean,
             preclean_files(
               list.files(en_data_dir, pattern = '*.txt')
             ))
