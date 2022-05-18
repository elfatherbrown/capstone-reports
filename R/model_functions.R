source("R/clean_texts.R")
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
    unkify(model_dt, .) %>%
    as.data.table() %>%
    group_by(sentence_id, order) %>%
    nest() %>%
    as_tibble() %>%
    pmap_dfr(function(sentence_id, order, data, ...) {
      o <- order
      tibble(sentence_id,
             order,
             p = model_dt[order == o][data, on = .(n_gram), nomatch = 0, verbose =
                                        TRUE] %>%
               .[, .(prob = sum(prob) + sum(backoff))] %>%
               .$prob)
    })
}

unkify <- function(model_dt, parsed_text,cols='n_gram',tok_unk=TOKEN_UNK) {
  unks <- parsed_text[order == 1] %>%
    .[!n_gram %chin% model_dt[order == 1, n_gram]] %>%
    .[, .(n_gram)] %>%
    .[, .(n_gram = str_replace_all(n_gram, c("(\\W)" = "\\\\\\1",
                                             '\\\\' = '\\\\')) %>%
            paste("\\b", ., "\\b"))]
#
#   parsed_text[, n_gram := str_replace_all(n_gram,
#                                          setNames(rep_along(unks$n_gram,
#                                                             paste0(" ", TOKEN_UNK, " ")),
#                                                   unks$n_gram))]

  parsed_text[, (cols) := str_replace_all,setNames(rep_along(unks$n_gram,
                                                             paste0(" ", tok_unk, " ")),
                                                   unks$n_gram)]

  return(parsed_text[])


}

parse_to_ngram_table <- function(intext,
                                 with_bos = TRUE,
                                 with_eos = TRUE,
                                 order = 5) {
  intext <- intext %>%
    tibble(text = .)
  if (with_bos)
  {
    intext <-
      intext %>%  mutate(text = str_replace_all(text, "^", paste0(TOKEN_BOS, " ")))
  }
  if (with_eos)
  {
    intext <-
      intext %>% mutate(text = str_replace_all(text, "$", paste0(" ", TOKEN_EOS)))
  }
  rng <- 1:order
  intext %>%
    pmap_dfr(function(text, ...) {
      rng %>%
        map_dfr(function(order) {
          tibble(
            order = order,
            n_gram = tokenizers::tokenize_ngrams(text,
                                                 n = order,
                                                 lowercase = FALSE) %>% unlist()
          )
        })
    }, .id = "sentence_id") %>%
    drop_na() %>%
    as.data.table()
}

#' unkify a data.table as ngrams
#' Explodes a large data table into its corresponding ngrams and unks it acording
#' to the unigrams available as compared to the inputed model
#'
#' @param unked_text_dt
#' A one row per text data.table
#' @return
#' @export
#'
#' @examples
unkify_all <- function(unked_text_dt, model_dt, order = 5,tok_unk=TOKEN_UNK) {
  tdt <- unked_text_dt
  tdt[, chunk := ceiling(.I %% .N / 1000)] %>%
    group_by(chunk) %>%
    nest() %>%
    as_tibble() %>%
    furrr::future_pmap(function(chunk, data, ...) {
      data %>%
        unkify(parsed_text = .,
               model_dt = model_dt,
               tok_unk = tok_unk)
    }) %>%
    rbindlist()
}

n_gramify_all <- function(text_dt, model_dt, order = 5) {
  tdt <- text_dt
  rows_per_chunk <- 500

  r <- tdt[, chunk := ceiling(.I %% .N / rows_per_chunk)] %>%
    group_by(chunk) %>%
    nest() %>%
    as_tibble() %>%
    future_pmap(function(chunk, data, ...) {
      r <-  data$text %>%
        parse_to_ngram_table(., order = order) %>%
        mutate(sentence_id = as.numeric(sentence_id) + ((chunk - 1) *
                                                          rows_per_chunk)) %>%
        as.data.table()
      return(r)
    }, .id = 'sentence_id_2', .options = furrr_options(seed = TRUE)) %>%
    rbindlist(.)

  #correcting final chunk sentence id
  final_sentence_id = r[sentence_id > 0, max(sentence_id)] + 1
  r[sentence_id < 0, sentence_id := final_sentence_id][]

  return(r)
}




scratch <- function() {
  unks <-
    parsed_text[order == 1][!n_gram %chin% model_dt[order == 1, n_gram]] %>%
    .[, .(n_gram)] %>%
    .[, .(n_gram = str_replace_all(n_gram, c("(\\W)" = "\\\\\\1",
                                             '\\\\' = '\\\\\\\\')) %>% paste("\\\\b", ., "\\\\b"))]

}

evaluate_ngram <- function(model_dt, ngram_dt) {
  model_dt[ngram_dt, on = .(order, n_gram), nomatch = NULL][, .(p = sum(prob), b =
                                                                  sum(backoff))][, t := p + b][]
}

corpus_probabilities_by_sentence <- function(model_dt, corpus_dt) {
  kenlm_evaluate(corpus_dt,model_dt)
  # corpus_dt %>%
  #   group_by(sentence_id) %>%
  #   nest() %>%
  #   as_tibble() %>%
  #   future_pmap(function(sentence_id, data, ...) {
  #     ddt <- evaluate_ngram(model_dt, data)
  #     ddt[, sentence_id := sentence_id][]
  #     return(ddt)
  #   }) %>%
  #   rbindlist()
}


corpus_probability <- function(corpus_probs_by_sentence) {
  corpus_probs_by_sentence %>%
    .[, .(
      probability = sum(p),
      backoff = sum(b),
      total = sum(t)
    )]
}
