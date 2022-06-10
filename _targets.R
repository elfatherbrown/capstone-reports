# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
pks <- c(
  "tibble",
  'data.table',
  'textclean',
  'rsample',
  'tarchetypes',
  'doParallel',
  'tidyverse',
  'dplyr',
  'tidyr',
  'ggplot2',
  'tibble',
  'readr',
  'purrr',
  'stringr',
  'furrr',
  'futile.logger',
  'glue',
  'LaF',
  'duckdb',
  'plotly'
) # Load other packages as needed. # nolint
suppressPackageStartupMessages(xfun::pkg_attach2(pks))

# Set target options:
tar_option_set(packages = pks,
               # packages that your targets need to run
               format = "qs") # default storage format
# Set other options as needed.)
options(datatable.verbose = FALSE)
# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Load the R scripts with your custom functions:
lapply(list.files("R", full.names = TRUE, recursive = TRUE), source)
# source("other_functions.R") # Source other scripts as needed. # nolint

future::plan(future::multisession(workers = 6))
dummy <- value(lapply(
  seq_len(nbrOfWorkers()),
  FUN = function(ii) {
    future(NULL, packages = "data.table", earlySignal = TRUE)
  }
))

library(futile.logger)
flog.appender(appender.file('targets.log'))
scratch_plot <- function() {
  tar_read(evaluate_models) %>%
    ggplot(aes(y = perplexity_including_oo_vs, x = order, fill = case)) +
    geom_col() +
    facet_wrap(. ~ evaluated_on) +
    theme(axis.text.x = element_text(
      angle = -30,
      vjust = 1,
      hjust = 0
    ))
}
# Replace the target list below with your own:
list(
  tar_files(
    origin,
    list.files(en_data_dir, pattern = "^en_.*(blogs|news|twitter).txt$", full.names = TRUE)
  ),
  tar_target(precleaned,
             preclean_files(
               str_replace_all(origin, ".*/([^/]+)$" , "\\1")
             ),
             format = "file"),
  tar_target(sentenced,
             sentencify(precleaned),
             format = 'file'),
  tar_target(sentenced_clean,
             {
               done <- sentenced %>%
               load_text_as_data_table() %>%
                   clean_texts_dt()%>%
                 group_by(file) %>%
                 nest() %>%
                 as_tibble()

               final <- tar_read(sentenced) %>%
                 enframe(name=NULL,value="fpath") %>%
                 mutate(ifname=fs::path_file(fpath),
                        ofname=paste0(clean_files_dir,'/clean_',ifname)) %>%
                 select(ifname,ofname) %>%
                 inner_join(done,
                            by=c('ifname'='file'))
               final %>%
                 pwalk(function(ofname,data,...){
                   readr::write_lines(x = data$text,file = ofname)
                 })
               return(final$ofname)
             },
             format = 'file'),
  tar_target(
    sentenced_clean_lower,
    sentenced_clean %>% map_chr(function(x) {
      ret <- readr::read_lines(x) %>%
        future_map_chr( ~ str_to_lower(.x))
      of <- str_replace_all(x, ".*/([^/]+)$" , "\\1")
      of <- paste0(clean_files_dir, '/lower_', of)
      write_lines(ret, of)
      return(of)
    }),
    format = "file"
  ),
  tar_target(corpus_f, {
    of <- filename_in("corpus_f")
    cat_files(sentenced_clean, ofile = of)
    return(of)
  },
  format = "file"),
  tar_target(corpus_f_l, length(readr::read_lines(corpus_f))),
  tar_target(corpus_f_lower,
             {
               of <- filename_in("corpus_f_lower")
               cat_files(sentenced_clean_lower,
                         ofile = of)
               return(of)
             },
             format = 'file'),
  tar_target(corpus_f_lower_l, length(readr::read_lines(corpus_f_lower))),
  tar_target(sample_size, c(0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.9,1)),
  tar_target(samples_f,
             {
               make_splits_to_files_tibble(
                 corpus_file = corpus_f,
                 corpus_length = corpus_f_l,
                 sample_size = sample_size
               ) %>%
                 mutate(case = 'no')
             },
             pattern = map(sample_size)),
  tar_target(samples_f_lower,
             {
               make_splits_to_files_tibble(
                 corpus_file = corpus_f_lower,
                 corpus_length = corpus_f_lower_l,
                 sample_size = sample_size
               ) %>%
                 mutate(case = 'lower')
             },
             pattern = map(sample_size)),
  tar_target(
    samples_as_files_tibble,
    bind_rows(samples_f, samples_f_lower)
  ),
  tar_target(
    rsample_splits,
    {
      make_rsample_split_with_sizes_tibble(
        samples_as_files_tibble$fname,
        samples_as_files_tibble$sample_size,
        samples_as_files_tibble$case
      )
    },
    pattern = map(samples_as_files_tibble),
    iteration = "list"
  ),
  tar_target(splits_as_files,
             {
               rsample_splits %>%
                 splits_to_files_tibble()
             },
             pattern = map(rsample_splits)),
  tar_target(prune, c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 20, 30, 40)),
  tar_target(order, c(3, 4, 5, 6)),

  tar_target(models_as_file_tibble,
             {
               splits_as_files %>%
                 filter(split == "training") %>%
                 pmap_dfr (function(sample_size, case, fname, ...) {
                   ofname <- create_kenlm_arpa(
                     fname,
                     outfile = glue("kenlm_{sample_size}_{case}_"),
                     max_order = order,
                     prune = prune
                   )

                   tibble(
                     order = order,
                     sample_size = sample_size,
                     case = case,
                     prune = prune,
                     fname = ofname,
                     size = fs::file_size(ofname)
                   )
                 })

             },
             pattern = cross(prune, order)),
  tar_target(
    models_splits_matrix,
    splits_as_files %>%
      rename(text_file = fname) %>%
      inner_join(
        models_as_file_tibble %>%
          rename(model_file = fname),
        by = c("sample_size", "case")
      )
  ),
  tar_target(evaluate_on, c("testing")),
  tar_target(evaluate_models,
             {
               tmatrix <- models_splits_matrix %>%
                 filter(split == evaluate_on)
               tmatrix %>%
                 pmap_dfr(function(sample_size,
                                  case,
                                  order,
                                  prune,
                                  text_file,
                                  model_file,
                                  ...) {

                   r <- kenlm_evaluate(text_file, model_file)

                   tibble(model_file = model_file,
                          evaluated_on = evaluate_on,
                          sample_size,
                          case,
                          order,
                          prune) %>%
                     bind_cols(r %>% pluck("summary_scores"))
                 })
             },
             pattern = map(evaluate_on)),
  tar_target(
    consolidated_evaluations,
    evaluate_models %>%
      inner_join(models_as_file_tibble) %>%
      mutate(across(starts_with("perpl"), as.numeric))
  ),
  tar_target(
    chosen_language_model,
    tar_read(consolidated_evaluations) %>%
      filter(size < fs::as_fs_bytes("400M") &
               evaluated_on == 'testing') %>%
      arrange(desc(size), perplexity_including_oo_vs) %>%
      slice_min(perplexity_including_oo_vs) %>%
      pull(model_file) %>%
      load_arpa_as_data_table() %>%
      select(-skip, -lines, -seq_id) %>%
      drop_na() %>%
      mutate(prob = as.numeric(prob)) %>%
      as.data.table()
  ),
  tar_target(main_table_name,'chosen_model'),
  tar_file(model_as_database_file,
           {
             model_name <- main_table_name
             model_file <-
               glue("{model_data_dir}/{model_name}.ddb.sql")
             write_dset(model_file = model_file,
                        dset = chosen_language_model,
                        model_name = model_name)

             return(model_file)
           })
)
