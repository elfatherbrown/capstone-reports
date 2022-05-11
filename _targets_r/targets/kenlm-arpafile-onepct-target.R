
tar_target(kenlm_arpafile_onepct,
             create_kenlm_arpa(
               source_file = training_as_file_onepct,
               outfile = 'kenlm_model_onepct'
                               ),
                               
             format='file')

