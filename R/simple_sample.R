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
simple_sample <- function(thefiles,pct,odir=clean_files_dir){
  thetext <- load_text_as_data_table(thefiles)
  ofile <- glue::glue("sample_for_testing_{pct}_pct.txt")
  otext <- thetext[sample(seq.int(1:.N),size = .N*pct),]
  fwrite(x = otext$text,file = ofile)
  return(ofile)
}
