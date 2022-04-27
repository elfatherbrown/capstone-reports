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
                        c_files_dir = clean_files_dir,
                        outfile_tag="") {
  ins <- paste0(s_files_dir, '/', files)
  outs <- paste0(c_files_dir, '/', outfile_tag,files)

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
                          str_remove_all(fixed(TOKEN_BOS)) %>%
                          str_remove_all(fixed(TOKEN_EOS)) %>%
                          str_remove_all(fixed(TOKEN_UNK)) %>%
                          str_remove_all(., fixed(''))

                        readr::write_file(x = r, file = y)

                        rm(r)
                        return(0)
                      }, .progress = FALSE)
  return(outs)
}

