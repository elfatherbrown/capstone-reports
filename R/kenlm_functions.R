

KENLM_EXEC = '/home/alex/bin/kenlm/lmplz'

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
             readr::write_file(x = ., file = rfn, append = TRUE))
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
    if (length(res) != 0 && "status" == attributes(res) %>% names()) {
      stop(
        glue::glue(
          "KenLM could not be generated, {res} status is integer == {attributes(res)$status}"
        )
      )
    }
    return(outfile)
  }

load_arpa_as_data_table <- function(source_file, max_order = 5) {
  order_by_chunk <- source_file %>%
    read_lines(skip = 1, n_max = max_order) %>%
    map_dfr(
      .,
      ~ tibble::tibble_row(
        order = str_replace(.x, "ngram ([0-9])=.*", "\\1") %>% parse_number(),
        lines = str_replace(.x, "ngram [0-9]=([0-9]+)", "\\1") %>% parse_number(),

      )
    ) %>%
    filter(lines != 0) %>%
    mutate(csum = cumsum(lines),
           skip = if_else(is.na(lag(order)),
                          max_order + 3,
                          lag(cumsum(lines)) + max_order + (2 * order) + 1)) %>%
    pmap(function(order, lines, csum, skip, ...) {

      read_tsv(
        source_file,
        n_max = lines,
        skip = skip,
        col_names = c("prob", 'n_gram', 'backoff'),
        num_threads = 4
      ) %>%
        mutate(order = order,
               skip=skip,
               lines=lines,
               seq_id=seq_along(n_gram)) %>%
        relocate(order, .before = 1) %>%
        as.data.table()
    } )%>%
    rbindlist()
}
