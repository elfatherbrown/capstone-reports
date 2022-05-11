source("R/sentencify.R")

  tar_target(
    sentenced_files,
    sentencify(precleaned_files = preclean),
    
    format = "file"
  )
