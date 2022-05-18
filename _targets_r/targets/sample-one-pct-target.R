source("R/simple_sample.R")
source("R/load_text_as_table.R")

tar_target(sample_one_pct,
           simple_sample(sentenced_files,pct = 0.01),
           format = "file")

  
