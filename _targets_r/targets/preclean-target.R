source("R/preclean_functions.R")
source("R/kenlm_functions.R")
# If you want to actually load the text into R, youd do this.
# list(tar_target(preclean, 
#     preclean_files(
#     list.files(en_data_dir,pattern = '*.txt')),
#     format="file"
# ),
# tar_target(precleaned_text, 
#     load_clean_text(),
#     format="qs"
# ))
# 
# But we prefer generating the model with kenlm
# 

  list(tar_target(preclean,
             preclean_files(
               list.files(en_data_dir,pattern = '*.txt')
             ),
             format="file"
  ),
  tar_target(single_cleantext_file,
             create_single_cleantext_file(
               source_files = tar_read(preclean)
             ),
             format="file"
  ),
   tar_target(kenlm_arpa_file,
             create_kenlm_arpa(tar_read(single_cleantext_file)),
             format = 'file')
  )
 

