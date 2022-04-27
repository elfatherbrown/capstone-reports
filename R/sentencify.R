sentencify <- function(precleaned_files,
                                 dir = clean_files_dir) {
  ret <- precleaned_files %>%
    furrr::future_map(function(x) {
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


  ret[, chunk := ceiling(.I %% .N / 10000)]
  gc()
  allfiles <- ret %>%
    distinct(fname)%>%
    pull(fname) %>%
    paste0(dir, '/sentences_', .)

  walk(allfiles,  function(x){
        if (fs::file_exists(x)) {
          fs::file_delete(path = x)
        }
      })
  out_fnames <- ret %>%
    group_by(chunk, fname) %>%
    nest() %>%
    as_tibble() %>%
    pmap_chr(function(chunk, fname, data, ...) {
      ofname <- glue::glue("{dir}/sentences_{fname}")
      sof <-
        tokenizers::tokenize_sentences(data$text, simplify = TRUE) %>%
        unlist()
      sof %>%
        readr::write_lines(x = . ,
                           append = TRUE,
                           file = ofname)
      rm(sof)
      return(ofname)
    })
  gc()
  return(unique(out_fnames))
}


clean_texts <- function(chunk_of_text) {
  chunk_of_text %>% textclean::replace_white() %>%
    textclean::replace_contraction() %>%
    textclean::replace_word_elongation() %>%
    textclean::replace_emoji_identifier() %>%
    textclean::replace_html() %>%
    textclean::replace_curly_quote() %>%
    textclean::replace_non_ascii() %>%
    textclean::replace_url() %>%
    textclean::replace_symbol(at = FALSE) %>%
    textclean::replace_ordinal() %>%
    return()
}
