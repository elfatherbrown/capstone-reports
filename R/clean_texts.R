clean_texts_dt <- function(texts_dt){

texts_dt %>%
    as_tibble() %>%
    mutate(chunk = row_number() %/% (nrow(.) / 10)) %>%
    group_by(chunk) %>%
    nest() %>%
    furrr::future_pmap_dfr(function(chunk,data,...){
      data %>%
        mutate(
          text=clean_texts(data$text)
        ) %>% drop_na()
    }) %>%
    as.data.table()

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
    str_replace_all("[^[:alnum:]]", "") %>%
    return()
}


quotemeta <- function(string) {
  str_replace_all(string, "(\\W)", "\\\\\\\\1")
}
