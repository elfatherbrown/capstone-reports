source('./R/model_functions.R')
tar_target(
  unkified_devtest_onepct,
  tar_read(ngramified_devtest_onepct) %>%
    unkify_all(
      unked_text_dt = .,
      model_dt = tar_read(kenlm_model_onepct)
    )
  
)

