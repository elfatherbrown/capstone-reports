
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
    initialize = function(base_dir, tag) {
      self$base_dir <- base_dir
      self$tag <- tag
      if(dir.exists(path(base_dir, tag))){
        self$load_model(path(base_dir,tag))
      } else {
        self$scaffold_model_dir(path(base_dir,tag))
      }
    },
    scaffold_model_dir=function(path){

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

