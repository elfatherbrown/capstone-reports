create_our_splits <- function(thefile){
  load_text_as_data_table(thefile) %>%
  rsample::initial_split() %>%
    return()
  }
