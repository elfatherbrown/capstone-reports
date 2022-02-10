
# Week 2 legacy ==============================
load_all_of_it <- function() {
  all_of_it <<-
    fread(
      file = file_clean_text_col,
      header = TRUE,
      quote = "",
      sep = '\n'
    )
}

#' write_sampled_and_topkenized
#'
#' @return
#' @export
#'
#' @examples

old_write_sampled_and_tokenized <- function() {
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

old_read_sampled_and_tokenized <- function() {
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

read_sampled_and_tokenized_m <-
  memoise::memoise(read_sampled_and_tokenized)


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
sample_files <- function(seed = 1212,
                         prop = 0.33,
                         preprocess = function(char_vector) {
                           return(char_vector)
                         },
                         postprocess = function(data) {
                           data %>%
                             filter(text != '') %>%

                             return()
                         },
                         ddir = clean_files_dir,
                         dpat = 'en_US*')
{
  set.seed(seed)
  random_chunk_reader <- function(x, pos) {
    if (prop != 1) {
      sample(x, length(x) * prop) %>%
        tibble(text = .) %>%
        mutate(rnum = row_number()) %>%
        preprocess() %>%
        postprocess()
    } else
    {
      x %>%
        tibble(text = x) %>%
        mutate(rnum = row_number()) %>%
        preprocess() %>%
        postprocess()
    }
  }

  ret <- list.files(ddir, pattern = dpat) %>%
    purrr::map_dfr(., function(fname) {
      tibble(file = fname,
             content = list(
               readr::read_lines_chunked(
                 paste0(ddir, "/", fname),
                 callback = DataFrameCallback$new(random_chunk_reader),
                 chunk_size = 20000
               )
             ))
    }) %>%
    unnest(cols = c(content)) %>%
    group_by(file) %>%
    mutate(rnum = row_number()) %>%
    ungroup() %>%
    mutate(from = str_remove(file, glue::glue("en_US\\.")),
           from = str_remove(from, "\\.txt")) %>%
    group_by(file) %>%
    ungroup()
  gc()
  return(ret)
}

on_the_fly_sampler <- function(seed = NULL,
                               prop = 0.33,
                               preprocess = function(char_vector) {
                                 return(char_vector)
                               },
                               delete_files = TRUE,
                               ngram_size = 2)
{
  thefiles <- sample_files(
    prop = prop,
    postprocess = function(d) {
      f <- tempfile(tmpdir = glue::glue("{out_data_dir}/chunks"))
      e <- d %>%
        r_tokenize(ngram_size) %>%
        count(ngram, name = 'ngram_count') %>%
        write_csv(x = .,
                  file = f)
      tibble(tempfile = f)
    }

  )
  set.seed(seed)
  files_sep <- sample(letters, 50) %>% paste0(collapse = '')
  files_fname <- glue::glue("THE_FILES_{files_sep}.csv")
  write.csv(thefiles,
            glue::glue("{out_data_dir}/{files_fname}"))

  thefiles %>%
    distinct(tempfile) %>%
    pull(tempfile) %>%
    map( ~ data.table::fread(file = .x, )) %>%
    data.table::rbindlist()
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
#' go about this since n* gram models can only rely in the last n words.
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


# Modeling Attempts week 3 ====================
# By :https://github.com/lgreski/datasciencectacontent/blob/master/markdown/capstone-simplifiedApproach.md

r_create_ngram_set <- function(data, order = 2) {

}

r_tokenize <- function(data, order, threads = 100) {
  # It can happen we get fed empty data. We return an empty set
  if (nrow(data) == 0) {
    return(tribble( ~ rnum,  ~ ngram,  ~ order))
  }
  data %>%
    as.data.table() %>%
    clean_text() %>%
    as_tibble() %>%
    unnest_ngrams(ngram, text, n = order)
}


r_count_and_followed_by <- function(data) {
  data %>%
    count(ngram, name = 'ngram_count') %>%
    mutate(prefix = str_remove(ngram, paste0(" [^ ]+$")))
}

r_count <- function(data) {
  data %>%
    count(ngram, name = 'ngram_count')
}

r_followed_by <- function(data) {
  data %>%
    mutate(prefix = str_remove(ngram, paste0(" [^ ]+$")))
}

v_last_words <- function(istring, how_many = 1) {
  how_many <- how_many - 1
  str_split(istring, ' ') %>%
    map_chr( ~ paste0(.x[length(.x) - how_many], collapse = " "))
}




r_odds_table <- function(parsed_ngram) {

}

odds_of_phrases <- function(ngram_model, phrases, max_order = 4) {
  counted_ngrams <- ngram_model$counted_ngrams

  total_ngrams <- ngram_model$total_ngrams

  the_gram <- r_parse_gram(phrases, max_order) %>% dtplyr::lazy_dt()
  backoff_counts <- counted_ngrams %>%
    select(ngram, ngram_count) %>%
    right_join(the_gram) %>%
    mutate(ngram_count = coalesce(ngram_count, as.integer(0)))

  counted_ngrams %>%
    select(prefix = ngram, prefix_count = ngram_count) %>%
    right_join(the_gram) %>%
    inner_join(backoff_counts) %>%
    mutate(
      prefix_count = if_else(is.na(prefix), total_ngrams, prefix_count),
      odds = coalesce(ngram_count / prefix_count, 0.0)
    )
}

#' get_model
#' Given an ngram dataset, it will give out a list with named elements
#' for the basic data needed for mle and other estimations
#'
#' @param ngrams
#'
#' @return
#' @export
#'
#' @examples
get_model <- function(ngrams) {
  ngrams <- ngrams %>% dtplyr::lazy_dt()
  counted_ngrams <- ngrams %>%
    count(ngram, name = "ngram_count") %>%
    drop_na()

  total_ngrams <- ngrams %>%
    filter(order == 1) %>%
    count() %>%
    pull(n)

  list(
    counted_ngrams = counted_ngrams,
    total_ngrams = total_ngrams,
    ngrams = ngrams
  )
}


tokenize_to <- function(all_of_it,
                        ddir = parquet_dir,
                        to = arrow::write_feather) {
  all_of_it %>%
    rename(text = V1) %>%
    group_by(chunk = row_number() %% 1000) %>%
    nest() %>%
    as_tibble() %>%
    future_pwalk(
      function(chunk, data, ddir, ...) {
        files_sep <-
          sample(letters, 50, replace = TRUE) %>% paste0(collapse = '')
        files_fname <-
          glue::glue("{ddir}/{files_sep}")
        data %>%
          as_tibble() %>%
          r_tokenize(5) %>%
          dtplyr::lazy_dt() %>%
          count(ngram, name = 'ngram_count') %>%
          as.data.table() %>%
          to(., files_fname)
        return(0)
      },
      .options = furrr_options(seed = 123),
      .progress = TRUE,
      ddir = ddir
    )
}


get_arrow_ds <- function(ddir, f) {
  options(arrow.skip_nul = TRUE)
  arrow::open_dataset(ddir, partitioning = NULL, format = f)
}

