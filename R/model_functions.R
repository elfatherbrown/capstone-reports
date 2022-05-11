#' ngrams_of_order
#' Counts ngrams of order
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
  parse_to_ngram_table(intext = text) %>%
               unkify(model_dt,.) %>%
   as.data.table() %>%
   group_by(sentence_id,order) %>%
   nest() %>%
   as_tibble() %>%
   pmap_dfr(function(sentence_id,order,data,...){
     o <- order
     tibble(sentence_id,
            order,
            p=model_dt[order==o][data,on=.(n_gram),nomatch=0,verbose=TRUE] %>%
              .[,.(prob=sum(prob)+sum(backoff))] %>%
              .$prob
            )
   })
}

unkify <- function(model_dt, parsed_text) {
  unks <-
    parsed_text[order == 1][!n_gram %chin% model_dt[order == 1, n_gram]][, n_gram] %>%
    paste0("\\b", ., "\\b") %>% paste0(collapse = '|')
  parsed_text %>%
    as_tibble() %>%
    mutate(n_gram = str_replace_all(n_gram, unks  , paste0(" ",TOKEN_UNK," ")))
}

parse_to_ngram_table <- function(intext,
                                 with_bos = TRUE,
                                 with_eos = TRUE) {
  intext <- intext %>%
    tibble(text = .)
  if (with_bos)
  {
    intext <-
      intext %>%  mutate(text = str_replace_all(text, "^", paste0(TOKEN_BOS," ")))
  }
  if (with_eos)
  {
    intext <-
      intext %>% mutate(text = str_replace_all(text, "$", paste0(" ",TOKEN_EOS)))
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

#' n-gramify all
#' Explodes a large data table into its corresponding ngrams and unks it acording
#' to the unigrams available as compared to the inputed model
#'
#' @param text_dt
#' A one row per text data.table
#' @return
#' @export
#'
#' @examples
n_gramify_unked_all <- function(text_dt,model_dt){
   tdt <- text_dt
   tdt[,chunk := ceiling(.I %% .N / 1000) ] %>%
     group_by(chunk) %>%
       nest() %>%
         as_tibble() %>%
     filter(chunk %in% c(1,2,6)) %>%
           furrr::future_pmap(function(chunk,data,...){
             data$text %>%
               parse_to_ngram_table(.) %>%
                 unkify(model_dt = model_dt,parsed_text = .)
             }) %>%
               rbindlist(s)
}

scratch <- function(){
  devtest <- tar_read(devtest_onepct)
  devtest[,chunk := ceiling(.I %% .N / 1000) ]
  devtest %>%
    group_by(chunk) %>%
    nest() %>%
    as_tibble() %>%
    furrr::future_pmap_dfr(
      function(chunk,data,...){
        text_prob_one(model_dt = model_dt,
                      data$text) %>%
          mutate(
            text_id
          )
      }
    )
}
