library(tidyverse)
library(tidytext)
library(dtplyr)
library(doParallel)
registerDoParallel(cores = 6)
raw_data_dir <- paste0(here::here(), "/../capstone raw data/")
en_data_dir <- paste0(raw_data_dir, "final/en_US")
board_folder = glue::glue("{Sys.getenv('PIN_LOCAL_FOLDER')}capstone")
board <- pins::board_folder(board_folder, versioned = TRUE)


#' write_sampled_and_topkenized
#'
#' @return
#' @export
#'
#' @examples

write_sampled_and_tokenized <- function() {
  pins::pin_write(
    board = board,
    x = sampled_files,
    name = "sampled_files",
    description = "Coursera capstone 33% sampled files",
    type = "arrow"
  )

  pins::pin_write(
    board = board,
    x = tok_corpora %>% as_tibble(),
    name = "tok_corpora",
    description = "Coursera capstone tokenized no stopwords",
    type = "arrow"
  )


  pins::pin_write(
    board = board,
    x = tok_corpora_bigram %>% as_tibble(),
    name = "tok_corpora_bigram",
    description = "Coursera capstone bigram tokenized no stopwords",
    type = "arrow"
  )

  pins::pin_write(
    board = board,
    x = tok_corpora_trigram %>% as_tibble(),
    name = "tok_corpora_trigram",
    description = "Coursera capstone trigram tokenized no stopwords",
    type = "arrow"
  )

  pins::pin_write(
    board = board,
    x = tok_corpora_freq %>% as_tibble(),
    name = "tok_corpora_freq",
    description = "Coursera capstone tokenized no stopwords",
    type = "arrow"
  )

  pins::pin_write(
    board = board,
    x = tok_corpora_bigram_freq,
    name = "tok_corpora_bigram_freq",
    type = "arrow"
  )

  pins::pin_write(
    board = board,
    x = tok_corpora_trigram_freq,
    name = "tok_corpora_trigram_freq",
    type = "arrow"
  )

  pins::pin_write(
    board = board,
    x = tok_corpora_freq_idf %>% as_tibble(),
    name = "tok_corpora_freq_idf",
    description = "Coursera capstone tokenized no stopwords",
    type = "arrow"
  )

  pins::pin_write(
    board = board,
    x = tok_corpora_bigram_freq_idf,
    name = "tok_corpora_bigram_freq_idf",
    type = "arrow"
  )

  pins::pin_write(
    board = board,
    x = tok_corpora_trigram_freq_idf,
    name = "tok_corpora_trigram_freq_idf",
    type = "arrow"
  )
}

read_sampled_and_tokenized <- function() {
  sampled_files <<- pins::pin_read(board = board,
                                   name = "sampled_files")
  tok_corpora <<- pins::pin_read(board = board,
                                 name = "tok_corpora")
  tok_corpora_bigram <<- pins::pin_read(board = board,
                                        name = "tok_corpora_bigram")
  tok_corpora_trigram <<- pins::pin_read(board = board,
                                         name = "tok_corpora_trigram")

  tok_corpora_freq <<- pins::pin_read(board = board,
                                      name = "tok_corpora_freq")
  tok_corpora_bigram_freq <<- pins::pin_read(board = board,
                                             name = "tok_corpora_bigram_freq")
  tok_corpora_trigram_freq <<- pins::pin_read(board = board,
                                              name = "tok_corpora_trigram_freq")


  tok_corpora_freq_idf <<- pins::pin_read(board = board,
                                          name = "tok_corpora_freq_idf")
  tok_corpora_bigram_freq_idf <<- pins::pin_read(board = board,
                                                 name = "tok_corpora_bigram_freq_idf")
  tok_corpora_trigram_freq_idf <<- pins::pin_read(board = board,
                                                  name = "tok_corpora_trigram_freq_idf")
}

#' Calculate Frequency
#'
#' @param data A data frame
#' @param colname The name of the column where ngrams are
#'
#' @return
#' @export
#'
#' @examples
calc_frequency <- function(data, colname) {
  colname_freq <- paste0(colname, "_freq")
  colname_total <- glue::glue("total_{colname}")
  colname_count <- glue::glue("{colname}_count")
  data %>%
    select(.data[[colname]]) %>%
    add_count(sort = TRUE, name = colname_total) %>%
    add_count(.data[[colname]], name = colname_count, sort = TRUE) %>%
    distinct() %>%
    mutate({
      {
        colname_freq
      }
    } := .data[[colname_count]] / .data[[colname_total]])
}


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
    } := str_remove_all(.data[[colname]], "ø|_+|[0-9\\.]")) %>%
    mutate({
      {
        colname
      }
    } := str_remove_all(.data[[colname]], "\"")) %>%
    mutate({
      {
        colname
      }
    } := str_replace_all(.data[[colname]], "&", " and "))

}

read_sampled_and_tokenized_m <-
  memoise::memoise(read_sampled_and_tokenized)


#' Sample 33% of original files
#'
#' @param seed
#'
#' @return
#' @export
#'
#' @examples
#'
#'
sample_files <- function(seed = 1212, prop = 0.33)
{
  set.seed(seed)
  random_chunk_reader <- function(x, pos) {
    tibble(text = sample(x, length(x) * prop))
  }

  list.files(en_data_dir) %>%
    purrr::map_df(., function(fname) {
      tibble(file = fname,
             content = list(
               readr::read_lines_chunked(
                 paste0(en_data_dir, "/", fname),
                 callback = DataFrameCallback$new(random_chunk_reader)
               )
             ))
    }) %>%
    unnest(cols = c(content)) %>%
    group_by(file) %>%
    mutate(rnum = row_number()) %>%
    ungroup() %>%
    mutate(from = str_remove(file, "en_US\\."),
           from = str_remove(from, "\\.txt")) %>%

    group_by(file) %>%
    clean_text() %>%
    ungroup() %>%
    data.table::as.data.table()
}

#' Creates an n-gram set wih proportion and log(proportions). Asumes input is
#' the format as output by sample_files
#'
#' @param file_sample
#' @param n
#'
#' @return
#' @export
#'
#' @examples
#'
create_ngram_set <- function(file_sample, n = 1) {
  if (n > 1) {
    outwords = paste0("word", 1:n)
  } else {
    outwords = "word"
  }
  file_sample %>%
    unnest_ngrams(
      input = "text",
      output = "ngram",
      n = n,
      to_lower = TRUE,
      format = 'text'
    ) %>%
    dtplyr::lazy_dt() %>%
    count(ngram, name = "ngram_count") %>%
    # separate(ngram, outwords, sep = " ", remove = FALSE) %>%
    drop_na() %>%
    mutate(prefix = str_remove(ngram, " [^ ]+$")) %>%
    as_tibble()

}

#' Given a gram string and an n, returns the tibble of ngrams constructable from it.
#'
#'
#' @param gram
#' @param n
#'
#' @return
#' @export
#'
#' @examples
parse_gram <- function(phrase, n) {
  gram <- phrase %>% trunc_phrase(n)
  wcount <- str_count(gram, pattern = " ") + 1
  if (n > wcount) {
    stop("Cant parse an ngram where order n is larger than word count")
  }
  if (n > 1) {
    outwords = paste0("word", 1:n)
  } else {
    outwords = "word"
  }
  sofar <- gram %>%
    tokenizers::tokenize_ngrams(lowercase = TRUE,
                                n = n,
                                ngram_delim = ' ')
  sofar %>%
    pluck(1) %>%
    enframe() %>%
    select(ngram = value) %>%
    separate(ngram, outwords, sep = " ", remove = FALSE) %>%
    mutate(prefix = str_remove(ngram, " [^ ]+$"))
}

#' trunc_phrase
#'
#' Extracts the last n words from a sentence. This is the only real way to
#' go about this since n* gram models can only relky in the last n words.
#'
#' @param i_string
#' @param n
#'
#' @return
#' @export
#'
#' @examples
trunc_phrase <- function(i_string, n = 3) {
  i_string %>%
    str_split(., ' ') %>%
    unlist() %>%
    .[(length(.) - (n - 1)):length(.)] %>%
    paste0(., collapse = ' ')
}

#' Bootstrap data
#' Will sample prop lines from the raw files, clean the output and generate as
#' many datasets (up to three now) as in the order vector
#'
#' @param prop
#' @param copy_to_parent
#' @param order
#'
#' @return
#' @export
#'
#' @examples
bootstrap_data <- function(prop = 0.33,
                           copy_to_parent = TRUE,
                           order = 1:3) {
  sampled_files <- sample_files(prop = prop)
  monograms <-
    sampled_files %>%
    create_ngram_set()

  bigrams <-
    sampled_files %>%
    create_ngram_set(2)

  trigrams <-
    sampled_files %>%
    create_ngram_set(3)

  if (copy_to_parent) {
    sampled_files <<- sampled_files
    monograms <<- monograms
    bigrams <<- bigrams
    trigrams <<- trigrams
  }

  return(
    list(
      sampled_files = sampled_files,
      monograms = monograms,
      bigrams = bigrams,
      trigrams = trigrams

    )
  )

}


#' separate and count
#' Given a dataframe with an ngram plain text column, separates them into as many ngrams
#' as words appear in the first row. NO MIXED "n" for this function is intelligent
#'
#' @param ngrams
#'
#' @return
#' @export
#'
#' @examples
separate_and_count <- function(ngrams) {
  wcount <- ngrams %>%
    slice_head(n = 1) %>%
    pull(ngram) %>%
    str_count(pattern = " ") + 1
  if (wcount > 1) {
    outwords = paste0("word", 1:wcount)
  } else {
    outwords = "word"
  }
  ngrams %>%
    dtplyr::lazy_dt() %>%
    separate(ngram, outwords, sep = " ", remove = FALSE) %>%
    as_tibble() %>%
    count_ngram_order()
}

#' count ngram order
#' Given a dataframe with separated ngrams, counts each and adds count cols
#'
#' @param sep_ngrams
#'
#' @return
#' @export
#'
#' @examples
count_ngram_order <- function(sep_ngrams) {
  columns <- sep_ngrams %>%
    names() %>%
    .[str_detect(., '^word[0-9]$')]
  order <- length(columns)

  columns %>%
    walk(function(x) {
      sep_ngrams <<- sep_ngrams %>%
        add_count(across(all_of(x)), name = paste0(x, "_count"))
    })

  sep_ngrams
}


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

good_turing <- function(ngrams,k=5){
  sofar <- ngrams %>% nr_plus_one()

  N1 <- sofar %>% filter(r==1) %>% slice_head(n=1) %>% pull(Nr)
  sofar %>%
    mutate(
      r_star=(r+1) * (Nrp1/Nr),
      mle=r/sum(r),
      Pgt=if_else(r==0,N1/sum(r),r_star/sum(r)),
      Pgt_1=if_else(r<=k,Pgt,mle),
      Pgt_2=Pgt_1/sum(Pgt_1)
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
  arrs <- ngrams %>% count(r=ngram_count,name='Nr')
  arrs %>%
  full_join(arrs %>%
              select(r) %>%
              mutate(r = as.integer(r + 1)),
            by='r') %>%
    arrange(r) %>%
    mutate(Nrp1 = if_else(
      is.na(Nr),
      as.integer(0),
      if_else(is.na(lead(Nr)),
              as.integer(0),
              lead(Nr))
    )) %>%
  select(r,Nr,Nrp1) %>%
  right_join(
    ngrams,
    by=c("r"='ngram_count')
    ) %>%
  select(ngram,prefix,r,Nr,Nrp1)
}

c_star <- function(ngrams,k=5) {
  sofar <-ngrams %>%
    nr_plus_one()

  Nkp1 <- sofar %>%
    filter(r == k+1) %>%
    slice_head(n=1) %>%
    pull(Nr)

  if(is.na(Nkp1)){
    Nkp1=0
  }

  N1 <- sofar %>% filter(r==1) %>% slice_head(n=1) %>% pull(Nr)
  sofar %>%
    mutate(
      c_star=if_else(
        r > k, as.numeric(r),NA_real_
      )
    ) %>%
    mutate(
      c1=(r+1)*(Nrp1/Nr),
      c2=r*(((k+1)*Nkp1)/N1),
      cd1=1-(((k+1)*Nkp1)/N1),
      c_star=if_else(is.na(c_star),
                     (c1-c2)/cd1,
                     c_star
      )
    ) %>%
  select(-c1,-c2,-cd1)

}

# Refurbish plainly -------
# By :https://github.com/lgreski/datasciencectacontent/blob/master/markdown/capstone-simplifiedApproach.md

r_create_ngram_set <- function(data,order=2){
  data %>%
    select(text) %>%
    unnest_ngrams(ngram,text,n=order)
    mutate(
      prediction=v_last_word(ngram),
      base=str_remove(ngram,prediction) %>% str_trim()
    ) %>%
  relocate(ngram,base,prediction) %>%
  add_count(ngram,name='n_count') %>%
  add_count(base,name='b_count') %>%
  mutate(p_pred_given_base=n_count/b_count) %>%
  distinct()
}

v_last_word <- function(istring,pick=0){
  if(pick>0){
    pick=pick-1
  }
  str_split(istring, ' ') %>%
    map_chr(~paste0(.x[(length(.x)-pick):length(.x)],collapse=" "))
}


