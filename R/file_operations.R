do_operate_on_par_files <-
  function(input_files, tag, op, outdir = clean_files_dir) {
    input_files %>%
      tibble::enframe(value = 'origin') %>%
      mutate(fname = str_replace_all(origin, ".*/([^/]+)$" , "\\1")) %>%
      furrr::future_pmap_chr(function(origin, fname, ...) {
        of <- glue::glue("{outdir}/{tag}_{fname}")
        read_file(origin) %>%
          op(.) %>%
          write_file(., file = of)
        return(of)
      },
      outdir = outdir)
  }
do_operate_on_chunked_files <- function(input_files,tag,op,outdir=clean_files_dir){
  input_files %>%
    tibble::enframe(value = 'origin') %>%
    mutate(fname = str_replace_all(origin, ".*/([^/]+)$" , "\\1")) %>%
    furrr::future_pmap_chr(function(origin, fname, ...) {
      of <- glue::glue("{outdir}/{tag}_{fname}")
      read_file(origin) %>%
        op(.) %>%
        write_file(., file = of)
      return(of)
    },
    outdir = outdir)

}
files_lowercase <- function(input_files) {
  input_files %>%
    do_operate_on_par_files(
      .,
      tag = "lowercase",
      op =  function(data) {
        str_to_lower(data) %>% return()

      }
    )
}

filename_in <- function(file_name_glue_pat,
                        indir=clean_files_dir,
                        ext='txt'){
  tempfile(tmpdir = indir,
           pattern = glue("{file_name_glue_pat}_"),
           fileext = '.txt')
}


cat_files <- function(thefiles,ofile=FALSE){
  r <- thefiles %>%
  map_chr(
    ~shQuote(.x)
  ) %>%
    reduce(
      .f = function(ac,st){paste0(ac,' ',st)},
      .init = "cat "
    ) %>%
    system(intern=TRUE)
  if(ofile!=FALSE){
    write_lines(r,ofile)
    return(ofile)
  } else {
    return(r)
  }

}
