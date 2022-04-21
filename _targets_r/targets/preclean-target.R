source("R/preclean_functions.R")
list(tar_target(preclean, 
    preclean_files(
    list.files(en_data_dir,pattern = '*.txt')),
    format="file"
),
tar_target(precleaned_text, 
    load_clean_text(),
    format="qs"
))

