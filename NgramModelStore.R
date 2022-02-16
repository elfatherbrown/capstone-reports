#' NgramModelStore
#'
#' @description
#' description
#' @return the NgramModelStore class
#'
#' @noRd

NgramModelStore <- R6::R6Class(
  classname = "NgramModelStore",
  public = list (
    base_dir = NULL,
    tag = NULL,
    files = NULL,
    initialize = function(base_dir, tag) {
      self$base_dir <- base_dir
      self$tag <- tag
    },
    #' build_onegram_hash
    #' Try to build as tight as possible word/num hash to pack ngrams in R
    #'
    #' @param onegrams
    #' A dframe with at least a begins and ngram_count columns
    #'
    #' @return
    #' @export
    #'
    #' @examples
    build_onegram_hash =
      function(onegrams) {
        onegrams %>%
          select(ngram = begins) %>%
          distinct(ngram) %>%
          mutate(wid = seq_along(ngram)) %>% as.data.table()
      },
    #' make_model_store
    #' Creates a directory of model_data_dir/model_tag, the model hash int-word table
    #' and writes all assumed to be ngram datasets
    #'
    #' @param model_tag
    #' A string of a non-existing model in model_data_dir/model_tag
    #'
    #' @param unigrams
    #' The unigram dataset with at least a begins and ngram_count columns
    #'
    #' @param ngrams_ds
    #' A named list list(name=full_filename), where name is the table name (threegram, fourgram... ),
    #' and full_filename is athe name of the csv with order, begins, ends and ngram count columns.
    #' These will be packed, subsituted to be encoded against the hash table
    #'
    #' @return
    #' The directory name where the model is now stored
    #'
    #' @export
    #'
    #' @examples
    make_model_store = function(model_tag, unigrams, ngrams_ds) {

    }
  )
)

#' NgramModel
#'
#' @description
#' An ngram model with a backend file and some basic operations
#' @return the NgramModel class
#'
#' @noRd

NgramModel <- R6::R6Class(
  classname = "NgramModel",
  public = list (
    ds = NULL,
    N = NULL,
    fname = NULL,
    ogHash = NULL,
    order = NULL,
    initialize = function(themodel, ogHash) {
      self$ogHash <- ogHash
      self$ds <- themodel
    },
    odds_of = function(ngram_df) {
      ngram_df %>%
        inner_join(self$ds) %>%
        inner_join(
          self$ogHash$ogm %>%
            select(prefix_count = ngram_count, ngram = ends),
          by = c(ends = "ngram")
        ) %>%
        mutate(odds = log(ngram_count / prefix_count))
    },
    next_options = function(ngram_ds, maxopts = 40) {
      self$ds %>%
        inner_join(ngram_ds %>% select(begins)) %>%
        slice_max(ngram_count, n = maxopts) %>%
        select(begins, ends, ngram_count)
    },
    perplexity = function(odds_df) {
      N <- nrow(odds_df)
      odds_df %>%
        summarize(perplexity = (1 / exp(sum(odds))) ^ (1 / N))
    }

  )
)

#' testNgramModel
#'
#' @description
#' Testing for ngram models
#' @return the testNgramModel class
#'
#' @noRd

testNgramModel <- R6::R6Class(classname = "testNgramModel",
                              public = list (
                                NGM = NULL,
                                initialize = function() {
                                  self$NGM = NgramModel$new(paste0(out_data_dir, '/threegram.csv'))
                                }

                              ))

#' oneGramHash
#'
#' @description
#' The special monogram hash, as packed as possible.
#' @return the oneGramHash class
#'
#' @noRd

oneGramHash <- R6::R6Class(
  classname = "oneGramHash",
  inherit = R6P::Singleton,
  public = list (
    theHash = NULL,
    ogm = NULL,
    N = NULL,
    initialize = function(fname) {
      self$ogm <- fread(fname) %>%
        select(ngram = ends, ngram_count) %>%
        as.data.table() %>%
        data.table::setindex(ngram)

      self$theHash <- self$ogm %>%
        distinct(ngram) %>%
        mutate(wid = seq_along(ngram)) %>%
        as.data.table() %>%
        data.table::setindex(wid, ngram) %>%
        data.table::setindex(ngram) %>%
        data.table::setindex(wid)
      self$N = self$ogm %>% as.data.table() %>%  nrow()
    }

  )

)

#' NgramCorpus
#'
#' @description
#' Given a set of files, builds an ngram dataset
#'
#' @return the NgramCorpus class
#'
#' @noRd

NgramCorpus <- R6::R6Class(
  classname = "ngramCorpus",
  public = list (
    corpus = NULL,
    N = NULL,
    initialize = function(corpus = NULL,
                          basedir = NULL,
                          filenames = c("twogram.csv",
                                        "threegram.csv",
                                        "fourgram.csv",
                                        "fivegram.csv"),
                          seed = 123,
                          slice_maximum = NULL,
                          lines_maximum = NULL) {
      if (!is.null(corpus)) {
        self$corpus <- corpus
        self$set_N()
      } else {
        self$load(
          basedir = basedir,
          filenames = filenames,
          seed = seed,
          slice_maximum = slice_maximum,
          lines_maximum = lines_maximum
        )
      }
    },
    load = function(basedir = NULL,
                    filenames = c("twogram.csv",
                                  "threegram.csv",
                                  "fourgram.csv",
                                  "fivegram.csv"),
                    seed = 123,
                    slice_maximum = NULL,
                    lines_maximum = NULL) {
      set.seed(seed)
      if (is.null(basedir)) {
        basedir <- out_data_dir
      }

      p <- progressr::progressor(along = filenames)
      sliceif <- function(data) {
        if (is.null(slice_maximum)) {
          data
        } else {
          p(message="slicing")
          data %>%
            # data.table::setindex(begins) %>%
            group_by(begins) %>%
            slice_max(ngram_count,
                      n = slice_maximum,
                      with_ties = FALSE) %>%
            as.data.table()
        }
      }
      self$corpus <-  filenames %>%
        paste0("^", .) %>%
        map_chr( ~ list.files(basedir, .x, full.names = T)) %>%
        future_map_dfr(function(x)
        {

          p(x)
          if (is.null(lines_maximum)) {
            ngram_set <- fread(x) %>%  sliceif() %>% self$set_indexes()
          } else {
            ngram_set <-
              fread(x, nrows = lines_maximum) %>%
              data.table::setindex(begins) %>%
              sliceif() %>%
              self$set_indexes()
          }
           order <- ngram_set %>% slice_head(n = 1) %>% pull(order)
          N <- ngram_set %>% nrow()
          tibble(ngram_set = list(ngram_set),
                 order = order,
                 N=N)
        })

    },
    set_N = function() {
      self$N <- self$corpus %>% nrow()
    },
    set_indexes = function(dt) {
      dt %>%
        mutate(ngram_id=row_number()) %>%
        as.data.table() %>%
        data.table::setindex(begins) %>%
        data.table::setindex(ends) %>%
        data.table::setindex(begins,ends) %>%
        data.table::setindex(order)
    },
    get = function(order) {
      o <- order
      self$corpus %>% filter(order == o) %>% pull(ngram_set) %>% pluck(1)
    },
    fullmatches=function(dt,ngram_ds){
      dt[ngram_ds,on=.(begins,ends),nomatch=0] %>%
        select(-i.order) %>%
        as.data.table()
      },
#' begincounts
#' Adds counts for the begins part of an ngram by searching for the previous order
#' ngram count
#' @param ngram_ds
#'
#' @return
#' @export
#'
#' @examples
    begincounts=function(ngram_ds){
      fullmatches <- self$fullmatches(self$corpus,ngram_ds)
      fullmatches[,prefix:=str_remove(begins,' [^ ]+$')]
    begincounts <- fullmatches %>%
        .[
        order>2,
        #Reduce the begins part of the ngram to its base, and previous order
        .(begins=str_remove(begins,' [^ ]+$'),order=order-1),
      ] %>%
        self$corpus[.,.(order,begins,ngram_count),on=.(order,begins),nomatch=0] %>%
        .[,.(begins_count=sum(ngram_count)),by=.(order,begins)]
    fullmatches <- fullmatches[begincounts,on=.(prefix),nomatches=0]
      }
  )
)
