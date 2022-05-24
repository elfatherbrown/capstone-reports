#' splits_to_files_tibble
#' Takes an rsample split, creates the devtest and training set
#' and then outputs them to files
#'
#' @param split
#'
#' @return
#' @export
#'
#' @examples
splits_to_files_tibble <- function(split_tibble) {

  split <- split_tibble$split %>% pluck(1)
  train <- split %>%
    rsample::training() %>%
    mutate(id = row_number())
  trs <- sample.int(nrow(train), nrow(train) * .9)
  devtest <- train[!train$id %in% trs, ]
  train <- train[train$id %in% trs, ]
  test <- split %>% rsample::testing()
  ss <- split_tibble$sample_size
  otest <- filename_in(glue("testing_{ss}"))
  otrain <- filename_in(glue("training_{ss}"))
  odevtest <- filename_in(glue("devtest_{ss}"))
  test %>%
    pull(text) %>%
    write_lines(., otest)

  train %>%
    pull(text) %>%
    write_lines(., otrain)

  devtest %>%
    pull(text) %>%
    write_lines(., odevtest)
  tibble(
    sample_size=split_tibble$sample_size,
    case=split_tibble$case,
    split="training",
    fname=otrain
    ) %>%
      add_row(
        sample_size=split_tibble$sample_size,
        case=split_tibble$case,
        split="testing",
        fname=otest
      )%>%
    add_row(
      sample_size=split_tibble$sample_size,
      case=split_tibble$case,
      split="devtest",
      fname=odevtest
    )

}

#' Title
#'
#' @param corpus_file
#' @param corpus_length
#' @param sample_size
#' As percentage of lines
#'
#' @return
#' @export
#'
#' @examples
make_splits_to_files_tibble <-
  function(corpus_file, corpus_length, sample_size) {
    ll <- corpus_length
    flog.info("Building samples_f corpus is of length: %s", ll)
    lines <-sample.int(n = ll, size = ll * sample_size) %>% sort()
    tlines <- LaF::get_lines(corpus_file,lines)
    flog.info("Building samples_f corpus of size %s results in %s lines",
              sample_size,
              length(tlines))
    of <- filename_in(glue::glue("sample_", sample_size))
    write_lines(tlines, of)
    tibble(sample_size = sample_size, fname = of)
  }

make_rsample_split_with_sizes_tibble <-
  function(fname, sample_size, case) {
    flog.info("Splitting %s over %s case %s",
              fname,
              sample_size,
              case)
    tibble(
      sample_size,
      case,
      split = readr::read_lines(fname) %>%
                           tibble(text = .) %>%
                           rsample::initial_split() %>% list(),

           )
  }
