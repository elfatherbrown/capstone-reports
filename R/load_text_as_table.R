#' Load clean text
#'
#' Gets all files in ddir and maps each line to a row. It creates a column
#' so you can tell which file each line comes from.
#'
#' @param ddir
#' @param pattern
#'
#' @return
#' @export
#'
#' @examples
load_text_as_data_table <-
  function(thefiles) {
    flog.info(glue("loading {length(thefiles)} files as data.table"))
    begins <- Sys.time()
   ret <- thefiles %>%
      future_map(function(x) {
        fread(
          file = x,
          sep = '\n',
          quote = "",
          header = FALSE,
          col.names = 'text',
          showProgress = FALSE
        ) %>%
          dtplyr::lazy_dt() %>%
          mutate(file = str_replace(x, '.*/([^/]+)$', '\\1')) %>%
          mutate(text_id=seq_along(text)) %>%
          select(text_id,file,text) %>%
          as.data.table()
      },
      .progress = FALSE) %>%
      rbindlist()
    gc()
    flog.info(glue('loading {nrow(ret)} took {as.numeric(Sys.time()-begins)} seconds'))
    return(ret)
  }
