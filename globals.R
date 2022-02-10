## SETUP ========================

library(tidyverse)
library(tidytext)
library(dtplyr)
library(doParallel)
library(furrr)
library(future)
library(data.table)
library(progressr)
library(quanteda)
library(rsample)
registerDoParallel(cores = 4)
plan(future::multisession(workers = 4))
raw_data_dir <- paste0(here::here(), "/../capstone raw data/")
en_data_dir <- paste0(raw_data_dir, "final/en_US")
clean_files_dir <- paste0(raw_data_dir, "final/en_US/clean")
out_data_dir <- paste0(en_data_dir, '/', 'output_csvs')
board_folder = glue::glue("{Sys.getenv('PIN_LOCAL_FOLDER')}capstone")
board <- pins::board_folder(board_folder, versioned = TRUE)

inter_data_dir <- paste0(raw_data_dir, "final/en_US/intermediate")



file_clean_text_col <-
  paste0(clean_files_dir, '/', 'clean_text_column.csv')
parquet_dir <- paste0(clean_files_dir, '/', 'as_parquet')

#EXPERIMENTAL ==================================
#

#' bagofwords
#'
#' @description
#' A simple bag of words on a vector implementation
#'
#' @return the bagofwords class
#'
#' @noRd

### Bag of words -----------------------
bagofwords <- R6::R6Class(
  classname = "bagofwords",
  public = list (
    bag = NULL,
    initialize = function() {
      self$bag = c()
    },
    addString = function(a_string) {
      nu <- 1
      names(nu) <- a_string
      if (is.null(self$bag)) {
        self$bag <<- nu
        return()
      } else if (is.na(self$bag[a_string])) {
        self$bag[a_string] <<- nu
      } else {
        nu <- self$bag[a_string] + nu
        self$bag[a_string] <<- nu
      }

    },
    as_tibble = function() {
      self$bag %>%
        future_map2_dfr(
          names(.),
          .,
          ~ tibble::tibble_row(ngram = .x, ngram_count = .y),
          .progress = TRUE,
          .options = furrr_options(seed = TRUE)
        )
    }
  )
)

#' Title
#'
#' @param vec
#' @param a_string
#'
#' @return
#' @export
#'
#' @examples
add_to_vec <- function(vec, a_string) {
  nu <- 1
  names(nu) <- a_string
  if (is.null(vec)) {
    vec <- nu

  } else if (is.na(vec[a_string])) {
    vec[a_string] <- nu
  } else {
    nu <- vec[a_string] + nu
    vec[a_string] <- nu
  }
  return(vec)
}

### Katz backoff -----------------------

#' good turing
#' Given a dataframe with an ngram_count column, it will generate a dataframe with
#' r, r_star and n_r_plus_one
#'
#' @param ngrams
#'
#' @return
#' @export
#'
#' @examples
less_good_turing <- function(ngrams) {
  ngrams %>%
    dtplyr::lazy_dt() %>%
    rename(r = ngram_count) %>%
    count(r, name = "n_r") %>%
    arrange(r) %>%
    mutate(
      n_r_plus_one = if_else(!is.na(lead(n_r)), lead(n_r), 0),
      r_star = (r + 1) * (n_r_plus_one / n_r)
    ) %>%
    as_tibble()
}

good_turing <- function(ngrams, k = 5) {
  sofar <- ngrams %>% nr_plus_one()

  N1 <- sofar %>% filter(r == 1) %>% slice_head(n = 1) %>% pull(Nr)
  sofar %>%
    mutate(
      r_star = (r + 1) * (Nrp1 / Nr),
      mle = r / sum(r),
      Pgt = if_else(r == 0, N1 / sum(r), r_star / sum(r)),
      Pgt_1 = if_else(r <= k, Pgt, mle),
      Pgt_2 = Pgt_1 / sum(Pgt_1)
    )
}

#' katz_dr
#'
#' Given a good turing resultant datarame, will calculate a column with the katz
#' dr discount rate
#'
#' @param gt_ngrams
#' @param k
#'
#' @return
#' @export
#'
#' @examples
katz_dr <- function(gt_ngrams, k = 5) {
  gt_ngrams <- gt_ngrams %>% dtplyr::lazy_dt()
  n_k <- gt_ngrams %>%
    filter(r == k) %>%
    pull(n_r)

  n_k_plus_one <- gt_ngrams %>%
    filter(r == k) %>%
    pull(n_r_plus_one)

  n_one <-  gt_ngrams %>%
    filter(r == 1) %>%
    pull(n_r)

  gt_ngrams %>%
    mutate(
      d_r_num = (r_star / r) - (((k + 1) * n_k_plus_one) / n_one),
      d_r_den = 1 - (((k + 1) * n_k_plus_one) / n_one),
      d_r = d_r_num / d_r_den,
      C_katz = d_r * r,
      P_katz = C_katz / sum(C_katz, na.rm = TRUE)
    ) %>%
    as_tibble()
}

ngram_order <- function(ngrams) {
  ngrams %>%
    slice_head(n = 1) %>%
    pull(ngram) %>%
    str_count(., " ") + 1
}

train_katz <- function(ngrams, k = 5) {
  if (ngram_order(ngrams) == 1) {
    ngrams %>%
      mutate(C_katz = ngram_count,
             P_katz = C_katz / sum(C_katz))
  } else {
    ngrams %>%
      good_turing() %>%
      katz_dr(k = k) %>%
      select(ngram_count = r, C_katz) %>%
      right_join(ngrams)
  }
}

searcher <- function() {
  searchfor <-
    "hi there, how are you man! Ok ? lets get some beer!!!" %>%
    parse_gram(2)
  searchfor$ngram
  trigrams_trained %>% filter(ngram == searchfor$ngram)
  bigrams_trained %>% filter(ngram == searchfor$prefix)

}
#' Nr plus one matrix
#'
#' @param ngrams
#'
#' @return
#' @export
#'
#' @examples
nr_plus_one <- function(ngrams) {
  arrs <- ngrams %>% count(r = ngram_count, name = 'Nr')
  arrs %>%
    full_join(arrs %>%
                select(r) %>%
                mutate(r = as.integer(r + 1)),
              by = 'r') %>%
    arrange(r) %>%
    mutate(Nrp1 = if_else(
      is.na(Nr),
      as.integer(0),
      if_else(is.na(lead(Nr)),
              as.integer(0),
              lead(Nr))
    )) %>%
    select(r, Nr, Nrp1) %>%
    right_join(ngrams,
               by = c("r" = 'ngram_count')) %>%
    select(ngram, prefix, r, Nr, Nrp1)
}

c_star <- function(ngrams, k = 5) {
  sofar <- ngrams %>%
    nr_plus_one()

  Nkp1 <- sofar %>%
    filter(r == k + 1) %>%
    slice_head(n = 1) %>%
    pull(Nr)

  if (is.na(Nkp1)) {
    Nkp1 = 0
  }

  N1 <- sofar %>% filter(r == 1) %>% slice_head(n = 1) %>% pull(Nr)
  sofar %>%
    mutate(c_star = if_else(r > k, as.numeric(r), NA_real_)) %>%
    mutate(
      c1 = (r + 1) * (Nrp1 / Nr),
      c2 = r * (((k + 1) * Nkp1) / N1),
      cd1 = 1 - (((k + 1) * Nkp1) / N1),
      c_star = if_else(is.na(c_star),
                       (c1 - c2) / cd1,
                       c_star)
    ) %>%
    select(-c1, -c2, -cd1)

}



# IN ACTIVE USE ==================


#' Cleans text in a column, excludes rows with single lol, rt, _, a mention to class= or style=,
#' as those are just html
#'
#' @param data
#' @param colname
#'
#' @return
#' @export
#'
#' @examples
clean_text <- function(data, colname = "text") {
  if (nrow(data) == 0) {
    return(data)
  }
  data %>%
    filter(
      !str_detect(
        .data[[colname]],
        "^[0-9\\W]+$|^rt$|^lol$|^_+$|^[0-9\\._]{2,}$|class=\\.*|style=\\.*"
      )
    ) %>%
    mutate({
      {
        colname
      }
    } := str_remove_all(.data[[colname]], "ø|_+|[0-9\\.]|")) %>%
    mutate({
      {
        colname
      }
    } := str_remove_all(.data[[colname]], "\"")) %>%
    mutate({
      {
        colname
      }
    } := str_replace_all(.data[[colname]], "&", " and ")) %>%
    mutate({
      {
        colname
      }
    } := str_to_lower(.data[[colname]]))

}

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
clean_files <- function(files,
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
                        r %>%
                          rawToChar(.) %>%
                          stringi::stri_enc_toascii(.) %>%
                          str_replace_all(., "\032", "") %>%
                          str_remove_all(., fixed('')) %>%
                          stringi::stri_enc_toutf8() %>%
                          str_to_lower() %>%
                          str_remove_all('[0-9@#$%^&\\*\\(\\)-=_\\+,<>`\\[\\]\\{\\}"\'\\\\]')  %>%
                          str_remove_all('\u001') %>%
                          readr::write_file(x = ., file = y)
                        p(y)
                        return(0)
                      }, .progress = TRUE)
  return(outs)
}

#' get clean files
#'
#' @param ddir
#' The directory where the files are.
#'
#' @return
#' A data.table with all files in ddir concatenated with a column "text",
#' one row per line, and a column file.
#'
#' @export
#'
#' @examples
get_clean_files <- function(ddir = clean_files_dir,pattern='txt') {
  list.files(paste0(ddir)) %>%
    .[str_detect(., pattern)] %>%
    paste0(ddir, '/', .) %>%
    future_map_dfr(function(x) {
      fread(
        x,
        sep = '\n',
        quote = "",
        header = FALSE,
        col.names = 'text'
      ) %>% mutate(file = str_replace(x, '.*/([^/]+)$', '\\1')) %>%
        clean_text() %>%
        as.data.table()
    },
    .progress = TRUE)

}

#' get_clean_splits
#'
#' @return
#' An rsample::initial_split object
#' @export
#'
#'
get_clean_splits <- function() {
  get_clean_files() %>%
    initial_split(strata = file)
}

#' do_map_split_tokens
#'
#' The workforce for tokenizing. Given a table with a column text, will split to
#' ngrams. Paralel in chunks of 10 rows of text.
#'
#' @param all_text
#' @param ngram_size
#'
#' @return
#' @export
#'
#' @examples
do_map_split_tokens <- function(all_text, ngram_size = 2)
{
  files_sep <-
    sample(letters, 50, replace = TRUE) %>%
    paste0(collapse = '')
  files_sep <- paste0(files_sep,'_',ngram_size,'gram.csv')
  fname <- paste0(inter_data_dir, '/', files_sep)

  all_text %>%
    mutate(chunk = row_number()%/%(nrow(all_text)/10)) %>%
    group_by(chunk) %>%
    nest() %>%
    as_tibble() %>%
    future_pwalk(function(chunk, data) {

        r <- data %>%
          as_tibble() %>%
          unnest_ngrams(ngram, text, n = ngram_size) %>%
          count(ngram, name = "ngram_count") %>%
          as.data.table()
          fwrite(x = r,
                 file = fname,append = TRUE)
          rm(r)
        return(0)
      },
    .progress = TRUE,
    .options = furrr_options(seed = TRUE))
  return(fname)
}



do_model <- function(maxprop=0.25,file="/home/alex/Devel-nosync/OAS-projects/cursos/capstone reports/../capstone raw data/final/en_US/intermediate/cuvrmjsoxt_2_gram.csv") {
  infile <- fread(file )
  infile %>%
    dtplyr::lazy_dt() %>%
    filter(ngram!="") %>%
    group_by(ngram) %>%
    summarize(ngram_count=sum(ngram_count)) %>%
    mutate(
      ends=str_extract(ngram,"[^ ]+$"),
      begins=str_remove(ngram," [^ ]+$")
    ) %>%
    select(begins,ends,ngram_count) %>%
    group_by(begins) %>%
    slice_max(ngram_count,prop=maxprop) %>%
  as.data.table()
  }


#' parse_ngram
#' Returns a tibble with up to order ngrams of phrase
#'
#' @param phrase
#' @param order
#'
#' @return
#' @export
#'
#' @examples
parse_gram <- function(phrase, max_order = 3) {
  phrase <- tibble(text=phrase) %>% clean_text() %>% pull(text)
  wc <- phrase  |> str_count(" ") + 1
  sp <- phrase |> str_to_lower() |> str_split(" ") %>% .[[1]]
  if (max_order >= wc) {
    max_order = wc
  }
  begins <- wc - max_order + 1

  map_dfr(c(begins:wc),  ~ tibble_row(ngram = paste0(sp[.x:wc], collapse =
                                                       " "),
                                      order = wc - .x + 1)) %>%
    mutate(prefix = if_else(order > 1,
                            str_remove(ngram, paste0(" [^ ]+$")),
                            NA_character_))
}
