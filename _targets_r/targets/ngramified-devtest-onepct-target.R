source('./R/model_functions.R')
tar_target(ngramified_devtest_onepct,
tar_read(clean_devtest_onepct) %>% 
    n_gramify_all(text_dt = .,
                        model_dt = tar_read(kenlm_model_onepct))

)

