
library(tidyverse)
library(tidytext)
library(dtplyr)
library(doParallel)
registerDoParallel(cores=6)
raw_data_dir<-paste0(here::here(),"/../capstone raw data/")
en_data_dir<-paste0(raw_data_dir,"final/en_US")
board_folder=glue::glue("{Sys.getenv('PIN_LOCAL_FOLDER')}capstone")
board <- pins::board_folder(board_folder,versioned = TRUE)
## Helper functions for down the road

write_sampled_and_tokenized <- function() {
  pins::pin_write(board = board,
                  x = sampled_files,
                  name = "sampled_files",
                  description = "Coursera capstone 33% sampled files",
                  type="arrow")

  pins::pin_write(board = board,
                  x = tok_corpora %>% as_tibble(),
                  name = "tok_corpora",
                  description = "Coursera capstone tokenized no stopwords",
                  type="arrow")


  pins::pin_write(board = board,
                  x = tok_corpora_bigram%>% as_tibble(),
                  name = "tok_corpora_bigram",
                  description = "Coursera capstone bigram tokenized no stopwords",
                  type="arrow")

  pins::pin_write(board = board,
                  x = tok_corpora_trigram%>% as_tibble(),
                  name = "tok_corpora_trigram",
                  description = "Coursera capstone trigram tokenized no stopwords",
                  type="arrow")

  pins::pin_write(board = board,
                  x = tok_corpora_freq %>% as_tibble(),
                  name = "tok_corpora_freq",
                  description = "Coursera capstone tokenized no stopwords",
                  type="arrow")

  pins::pin_write(board = board,
                  x = tok_corpora_bigram_freq,
                  name = "tok_corpora_bigram_freq",
                  type="arrow" )

  pins::pin_write(board = board,
                  x = tok_corpora_trigram_freq,
                  name = "tok_corpora_trigram_freq",
                  type="arrow" )

  pins::pin_write(board = board,
                  x = tok_corpora_freq_idf %>% as_tibble(),
                  name = "tok_corpora_freq_idf",
                  description = "Coursera capstone tokenized no stopwords",
                  type="arrow")

  pins::pin_write(board = board,
                  x = tok_corpora_bigram_freq_idf,
                  name = "tok_corpora_bigram_freq_idf",
                  type="arrow" )

  pins::pin_write(board = board,
                  x = tok_corpora_trigram_freq_idf,
                  name = "tok_corpora_trigram_freq_idf",
                  type="arrow" )
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
                                             name ="tok_corpora_bigram_freq")
  tok_corpora_trigram_freq <<- pins::pin_read(board = board,
                                              name = "tok_corpora_trigram_freq")


  tok_corpora_freq_idf <<- pins::pin_read(board = board,
                                          name = "tok_corpora_freq_idf")
  tok_corpora_bigram_freq_idf <<- pins::pin_read(board = board,
                                                 name ="tok_corpora_bigram_freq_idf")
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
calc_frequency <- function(data,colname){
  colname_freq <- paste0(colname,"_freq")
  colname_total <- glue::glue("total_{colname}")
  colname_count <- glue::glue("{colname}_count")
  data %>%
    select(.data[[colname]]) %>%
    add_count(sort = TRUE,name = colname_total) %>%
    add_count(.data[[colname]],name=colname_count,sort=TRUE) %>%
    distinct() %>%
    mutate(
      {{colname_freq}} := .data[[colname_count]]/.data[[colname_total]]
    )
}


clean_text <- function(data,colname="text"){
  data %>%
    filter(!str_detect(.data[[colname]],"^[0-9\\W]+$|^rt$|^lol$|^_+$|^[0-9\\._]{2,}$|class=\\.*|style=\\.*")) %>%
    mutate( {{colname}} := str_remove_all(.data[[colname]],"ø|_+|[0-9\\.]")) %>%
    mutate( {{colname}} := str_remove_all(.data[[colname]],".*#[a-zA-Z0-9]+")) %>%
    mutate( {{colname}} := str_remove_all(.data[[colname]],"\"")) %>%
    mutate( {{colname}} := str_replace_all(.data[[colname]],"&","and"))

}

read_sampled_and_tokenized_m <-  memoise::memoise(read_sampled_and_tokenized)

random_chunk_reader<-function(x,pos){
  tibble(
    text=sample(x,length(x)*0.33)
  )
}
sampled_files<-list.files(en_data_dir)%>%
  purrr::map_df(.,function(fname){
    tibble(file=fname,
           content=list(readr::read_lines_chunked(paste0(en_data_dir,"/",fname),
                                                  callback = DataFrameCallback$new(random_chunk_reader))
           ))
  })%>%
  unnest(cols=c(content))%>%
  group_by(file)%>%
  mutate(rnum=row_number())%>%
  ungroup()%>%
  mutate(
    from=str_remove(file,"en_US\\."),
    from=str_remove(from,"\\.txt")
  )
