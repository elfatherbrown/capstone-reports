#' simple_sample
#' Directly sample from files by sentence (lines).
#'
#' @param files
#' Files to sample lines from
#'
#' @param pct
#' Percent of lines to sample from each file.
#'
#' @return
#' @export
#'
#' @examples
simple_sample <- function(thefiles, pct, odir = clean_files_dir) {
  options(readr.show_progress = FALSE)
  thetext <- load_text_as_data_table(thefiles)
  ofile <- glue::glue("{odir}/sample_for_testing_{pct}_pct.txt")

  otext <- thetext[sample(.N, size = .N * pct), ]
  write_lines(x = otext$text, file = ofile)
  return(ofile)
}
