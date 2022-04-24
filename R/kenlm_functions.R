library(xfun)
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
      walk( ~ readr::read_file(.x) %>%
              readr::write_file(x = ., file = rfn, append = TRUE))
    return(rfn)
  }
create_kenlm_arpa <-
  function(source_file,
           outfile="kenlm_after_prune.arpa",
           order = 5,
           prune = 40) {
    outfile <- glue::glue("{clean_files_dir}/{outfile}")
    res <- system(glue::glue("{KENLM_EXEC}  --order={order} ",
                      "--text='{source_file}' ",
                      "--arpa='{outfile}' ",
                      '--prune={prune} ',
                      '--skip_symbols'),
           intern = TRUE
           )
    if(length(res) != 0 && "status"==attributes(res) %>% names()){
      stop(glue::glue("KenLM could not be generated, {res} status is integer == {attributes(res)$status}"))
    }
    return(outfile)
  }

load_arpa_as_data_table <-function(source_file){

  }
