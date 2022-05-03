#' ngrams_of_order
#' Coutns ngrams of order
#'
#' @param model_dt
#' The data table representing the model with: order,n_gram,prob,backoff
#'
#' @param order
#' The order that we want
#'
#' @return
#' @export
#'
#' @examples
ngrams_of_order <- function(model_dt, ord) {
  model_dt[order == ord, .N]
}

max_order <- function(model_dt) {
  model_dt[order == max(order), order] %>% unique()
}



text_prob_one <- function(model_dt, text) {
  parse_to_ngram_table(intext = text)

}

unkify <- function(model_dt, parsed_text) {
  unks <- parsed_text[order==1][!n_gram %chin% model_dt[order==1,n_gram]][,n_gram] %>%
    paste0("\\b",.,"\\b")%>% paste0(collapse = '|')

  parsed_text %>%
    as_tibble() %>%
    mutate(stringr_rep = str_replace_all(n_gram, unks  , TOKEN_UNK))

  }

parse_to_ngram_table <- function(intext,
                                 with_bos = TRUE,
                                 with_eos = TRUE) {
  intext <- intext %>%
    tibble(text = .)
  if (with_bos)
  {
    intext <-
      intext %>%  mutate(text = str_replace_all(text, "^", TOKEN_BOS))
  }
  if (with_eos)
  {
    intext <-
      intext %>% mutate(text = str_replace_all(text, "$", TOKEN_EOS))
  }
  intext %>%
    pmap_dfr(function(text, ...) {
      1:5 %>%
        map_dfr(function(order) {
          tibble(
            order = order,
            n_gram = tokenizers::tokenize_ngrams(text,
                                                 n = order,
                                                 lowercase = FALSE) %>% unlist()
          )
        })
    }, .id = "sentence_id") %>%
      as.data.table()
}
