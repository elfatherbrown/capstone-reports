#' Create training and devset
#'
#' Splits training_dt into a devtest with 10% random rows and training
#' from the rest.
#'
#' @param training_dt
#'
#' @return
#' @export
#'
#' @examples
create_training_and_devset <- function(training_dt) {
  devsetids <- sample((training_dt[,.N]), size = training_dt[, .N] * 0.1)
  trainids <-
    setdiff(seq.int(from = 1, to = training_dt[, .N], by = 1), devsetids)
  list("training" = training_dt[trainids],
       "devtest" = training_dt[devsetids])
}
