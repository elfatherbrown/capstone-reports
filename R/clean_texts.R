clean_texts_dt <- function(texts_dt){

flog.info(glue("Cleaning dt of {nrow(texts_dt)} rows"))
  firstdif <- Sys.time()
finret <- texts_dt %>%
    as_tibble() %>%
    mutate(chunk = row_number() %/% (nrow(.) / 100000)) %>%
    group_by(chunk) %>%
    nest() %>%
    furrr::future_pmap_dfr(function(chunk,data,...){
      began <- Sys.time()
      ret <- data %>%
        mutate(
          text=clean_texts(data$text)
        ) %>% drop_na()
      flog.info(glue("Cleaning chunk {chunk} of {nrow(data)} rows ",
                     "took {as.numeric(Sys.time()-began)} ",
                     "process began {as.numeric(Sys.time()-firstdif)} ago"))

      return(ret)
    }) %>%
    as.data.table()
   flog.info(glue("Final elapsed is {as.numeric(Sys.time()-firstdif)}"))
   return(finret)
}

#' clean texts
#' Plain character vector of ONE element cleaning
#'
#' @param chunk_of_text
#'
#' @return
#' @export
#'
#' @examples
clean_texts <- function(chunk_of_text) {
  chunk_of_text %>% textclean::replace_white() %>%
    textclean::replace_contraction() %>%
    textclean::replace_word_elongation() %>%
    textclean::replace_emoji_identifier() %>%
    textclean::replace_html() %>%
    textclean::replace_curly_quote() %>%
    textclean::replace_non_ascii() %>%
    textclean::replace_url() %>%
    textclean::replace_symbol(at = FALSE) %>%
    textclean::replace_ordinal() %>%
    str_replace_all("[^[:alnum:] ]", "") %>%
    return()
}


quotemeta <- function(string) {
  str_replace_all(string, "(\\W)", "\\\\\\\\1")
}
