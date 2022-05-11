
source("R/clean_texts.R")
list(
  tar_target(
    clean_testing_onepct,
    clean_texts_dt(testing_onepct)
  ),
  tar_target(
    clean_training_onepct,
    clean_texts_dt(training_onepct)
  ),
  tar_target(
    clean_devtest_onepct,
    clean_texts_dt(devtest_onepct)
  )
  
)

