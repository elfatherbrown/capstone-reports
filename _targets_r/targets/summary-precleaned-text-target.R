library(tidyverse)
library(dtplyr)
tar_target(
  preclean_summary,
 tar_read("precleaned_text")  %>%
  dtplyr::lazy_dt() %>% 
  group_by(file=as_factor(file)) %>% 
  as_tibble() %>% 
  mutate(avlength=str_count(text)) %>% 
  summary() 
)
