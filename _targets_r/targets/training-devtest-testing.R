source("R/create_training_and_devset.R")

list(
  tar_target(
    testing_onepct,
    splits_of_onepct %>% 
      rsample::testing()
    ),
    tar_target(pre_training_onepct,
               splits_of_onepct %>%
                 rsample::training()
               ),
    tar_target(
      training_devtest_split_onepct,
      create_training_and_devset(pre_training_onepct)
    ),
    tar_target(training_onepct,
               purrr::pluck(training_devtest_split_onepct, "training")
               ),
    tar_target(devtest_onepct,
               purrr::pluck(training_devtest_split_onepct, "devtest")
               )
    
  )
  

