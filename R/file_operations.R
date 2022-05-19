do_operate_on <- function(input_files,tag,op,outdir=clean_files_dir) {

  input_files %>%
    tibble::enframe(value = 'origin') %>%
    mutate(
      fname=str_replace_all(origin,".*/([^/]+)$" ,"\\1")
    ) %>%
    furrr::future_pmap_chr(function(origin,fname,...){
      of <- glue::glue("{outdir}/{tag}_{fname}")
      read_file(origin) %>%
        str_to_lower(.) %>%
        write_file(.,file=of)
      return(of)
    },
    outdir=outdir)
}

files_lowercase <- function(input_files){
  input_files %>%
    do_operate_on(.,
                  "lowercase",
                  function(data){
                    str_to_lower(data) %>% return()

  })
  }
