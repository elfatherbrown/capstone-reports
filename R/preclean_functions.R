## PRECLEAN ===========
##
##
#' Cleans a set of files
#'
#'
#' @param files
#' Plain file names, no path
#'
#' @param s_files_dir
#' Source directory where files reside
#'
#' @param c_files_dir
#' Destination directory to put clean files. Will be overwritten if they exist.
#'
#' @return
#' Full path to each output file
#' @export
#'
#' @examples
preclean_files <- function(files,
                        s_files_dir = en_data_dir,
                        c_files_dir = clean_files_dir) {
  ins <- paste0(s_files_dir, '/', files)
  outs <- paste0(c_files_dir, '/', files)
  p <- progressor(along = ins)
  furrr::future_walk2(.x = ins,
                      .y = outs,
                      function(x, y) {
                        r <- readr::read_file_raw(x)
                        r[r == as.raw(0)] = as.raw(0x20)
                        r <- r %>%
                          rawToChar(.) %>%
                          stringi::stri_enc_toascii(.) %>%
                          str_replace_all(., "\032", "") %>%
                          stringi::stri_enc_toutf8() %>%
                          # str_remove_all('[0-9]')  %>%
                          str_remove_all('\u001') %>%
                          str_remove_all('\t') %>%
                          str_remove_all(fixed(TOKEN_BOS)) %>%
                          str_remove_all(fixed(TOKEN_EOS)) %>%
                          str_remove_all(fixed(TOKEN_UNK)) %>%
                          str_remove_all(., fixed(''))

                        readr::write_file(x = r, file = y)
                        p(y)
                        rm(r)
                        return(0)
                      }, .progress = FALSE)
  return(outs)
}

#' Load clean text
#'
#' Gets all files in ddir and maps each line to a row. It creates a column
#' so you can tell which file each line comes from.
#'
#' @param ddir
#' @param pattern
#'
#' @return
#' @export
#'
#' @examples
load_clean_text <-
  function(ddir = clean_files_dir,
           pattern = 'txt') {
    ret <- list.files(paste0(ddir)) %>%
      .[str_detect(., pattern)] %>%
      paste0(ddir, '/', .) %>%
      future_map(function(x) {
        fread(
          x,
          sep = '\n',
          quote = "",
          header = FALSE,
          col.names = 'text'
        ) %>%
          dtplyr::lazy_dt() %>%
          mutate(file = str_replace(x, '.*/([^/]+)$', '\\1')) %>%
          mutate(text_id=seq_along(text)) %>%
          select(text_id,file,text) %>%
          as.data.table()
      },
      .progress = TRUE) %>%
      rbindlist()
    gc()
    return(ret)
  }
