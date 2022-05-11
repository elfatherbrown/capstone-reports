





KENLM_EXEC = '/home/alex/bin/kenlm/lmplz'

create_file_from_data_table_set <- function(dt_set,
                                            outfilename,
                                            outdir = clean_files_dir) {
  ofile <- fs::path(outdir, outfilename)
  dt_set$text %>%
    readr::write_lines(x = ., file = ofile)
  return(ofile)
}

create_single_cleantext_file <-
  function(source_files,
           source_dir = clean_files_dir,
           fname = 'clean_text_one_file.txt') {
    rfn <- glue::glue("{source_dir}/{fname}")
    if (file.exists(rfn)) {
      fs::file_delete(path = rfn)
    }
    source_files %>%
      walk(~ readr::read_file(.x) %>%
             readr::write_file(x = ., file = rfn,
                               append = TRUE))
    return(rfn)
  }

create_kenlm_arpa <-
  function(source_file,
           outfile = "kenlm_after_prune.arpa",
           max_order = 5,
           prune = 40) {
    outfile <- glue::glue("{clean_files_dir}/{outfile}")
    res <- system(
      glue::glue(
        "{KENLM_EXEC}  --order={max_order} ",
        "--text='{source_file}' ",
        "--arpa='{outfile}' ",
        '--prune={prune} ',
        '--skip_symbols'
      ),
      intern = TRUE
    )
    if (length(res) != 0 &&
        "status" == attributes(res) %>% names()) {
      stop(
        glue::glue(
          "KenLM could not be generated, {res} status is integer == {attributes(res)$status}"
        )
      )
    }
    return(outfile)
  }


load_arpa_as_data_table <- function(source_file, max_order = 5) {
  options(readr.progress = FALSE,
          readr.show_col_types = FALSE)
  order_by_chunk <- source_file %>%
    readr::read_lines(skip = 1, n_max = max_order) %>%
    map_dfr(
      .,
      ~ tibble::tibble_row(
        order = str_replace(.x, "ngram ([0-9])=.*", "\\1") %>% parse_number(),
        lines = str_replace(.x, "ngram [0-9]=([0-9]+)", "\\1") %>% parse_number(),

      )
    ) %>%
    filter(lines != 0) %>%
    mutate(skip = if_else(
      is.na(lag(order)),
      max_order + 3,
      lag(cumsum(lines)) + max_order + (2 * order) + 1
    )) %>%
    ret <- pmap(function(order, lines, csum, skip, ...) {
      ret <- readr::read_tsv(
        source_file,
        n_max = lines,
        skip = skip,
        col_names = c("prob", 'n_gram', 'backoff'),
        quote = "",
        show_col_types = FALSE

      ) %>%
        mutate(
          order = order,
          skip = skip,
          lines = lines,
          seq_id = seq_along(n_gram)
        ) %>%
        relocate(order, .before = 1) %>%
        mutate(
          n_gram = str_replace_all(n_gram, '<unk>', TOKEN_UNK),
          n_gram = str_replace_all(n_gram, '<s>', TOKEN_BOS),
          n_gram = str_replace_all(n_gram, '</s>', TOKEN_EOS)
        ) %>%
        as.data.table()
      # If an ngram model has no backoff for any ngram,
      # kenlm removes the column. We add it here
      if ("backoff" %in% names(ret)) {
        return(ret)
      } else {
        ret[, backoff := 0.0]
        return(ret)
      }

    }) %>%
    data.table::rbindlist()
    setindexv(ret,"order")
    setindexv(ret,"n_gram")
    setindexv(ret,c("order","n_gram"))
    return(ret)
}
