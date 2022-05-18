library(futile.logger)
flog.appender(appender.file('targets.log'))

sentencify <- function(precleaned_files,
                       dir = clean_files_dir,
                       minchars = 5) {
  flog.info("started sentencification of %s files",
            length(precleaned_files))
  ret <- precleaned_files %>%
    furrr::future_map(function(x) {
      flog.appender(appender.file('targets.log'))
      flog.info("Reading %s", x)
      ir <- fread(
        file = x,
        sep = '\n',
        quote = "",
        header = FALSE,
        col.names = 'text'
      ) %>%
        mutate(fname = str_replace(x, '.*/([^/]+)$', '\\1'))
    }) %>%
    rbindlist()
  flog.info("Colective corpus of %s lines", NROW(ret))
  lines_per_chunk <-  5000
  ret[, chunk := ceiling(.I %% .N / lines_per_chunk)]
  flog.appender(appender.file('targets.log'))
  flog.info("Resulting in %s chunks", ret[chunk == max(chunk)][1]$chunk)
  gc()
  allfiles <- ret %>%
    distinct(fname) %>%
    pull(fname) %>%
    paste0(dir, '/sentences_', .)

  walk(allfiles,  function(x) {
    if (fs::file_exists(x)) {
      flog.appender(appender.file('targets.log'))
      flog.info("Deleting existing file %s", x)
      fs::file_delete(path = x)
    }
  })
  flog.info("Starting parallel chunk processing")
  out_fnames <- ret %>%
    group_by(chunk, fname) %>%
    nest() %>%
    as_tibble() %>%
    furrr::future_pmap_chr(function(chunk, fname, data, ...) {
      flog.appender(appender.file('targets.log'))
      flog.info("Starting chunk %s of %s lines for file %s",
                chunk,
                NROW(data),
                fname)
      ofname <- glue::glue("{dir}/sentences_{fname}")

      data <- data %>%
        filter(str_count(str_trim(text)) > minchars)

      outlines <- sentencify_simple(data$text) %>%
        unlist()
        readr::write_lines(x = outlines ,
                           append = TRUE,
                           file = ofname)
      flog.info("Wrote %s lines for chunk %s of %s lines for file %s",
                NROW(outlines),
                chunk,
                NROW(data),
                fname)

      return(ofname)
    })
  flog.info("Ended paralell processing, collecting garbage")
  gc()
  flog.info("Ended of sentencification process")
  return(unique(out_fnames))
}


sentencify_simple <- function(text) {
  text %>%
    textshape::split_sentence() %>%
    unlist()
}
