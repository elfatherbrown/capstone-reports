library(tokenizers)
#' clean_sentences
#'
#' Sentecifies the input text_data
#'
#' @param text_data
#' A data.table  with file,text columns
#'
#' @return
#' A data.table with same columns, but potentially more rows since sentences
#' are expanded
#'
#' @export
#'
sentencify <- function(text_data,odir=out_data_dir,ofile='corpus.csv'){
  ofile <- paste0(odir,'/',ofile)
  text_data %>%
  as_tibble() %>%
    mutate(chunk = row_number() %/% (nrow(.) / 10)) %>%
    group_by(chunk) %>%
    nest() %>%
    furrr::future_pwalk(
      function(chunk,data,...) {
        tibble(
          file = chunk,
          text = data$text %>% tokenizers::tokenize_sentences(
            simplify = TRUE)
        ) %>%
          mutate(sent_id=seq_along(text)) %>%
          as.data.table() %>%
          fwrite(x=.,
                 file=ofile,
                 append=TRUE,
                 showProgress = FALSE)
      },
      .options = furrr::furrr_options(seed=NULL,
                                      chunk_size = 10)
    )
  return(ofile)
}
