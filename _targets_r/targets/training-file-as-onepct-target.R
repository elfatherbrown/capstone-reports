source("R/kenlm_functions.R")

# KenLM requires a simple clean text file to build. 
# So we do that with our training set

  tar_target(training_as_file_onepct,
             create_file_from_data_table_set(
               clean_training_onepct,
               'training_corpus_onepct.txt'
             ),
             format="file")
             
