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
  'furrr'
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

origin_files <-
  list.files(clean_files_dir, pattern = "^en_.*(blogs|news|twitter).txt$")

sample_sizes = c(0.01, 0.05, 0.1, 0.2)
upper_lower = rlang::syms(c("all_sentenced_dirty_lowercase",
                            "all_sentenced_dirty"))
testing_training = c("testing", "training")
all_splits = c("testing", "training", "devtest")
training_splits = c("testing", "devtest")

simple_samples_pars <- crossing(sample_size = sample_sizes,
                                sample_target = upper_lower) %>%
  mutate(target_name = paste0("simple_samples_", sample_size, "_", sample_target) %>% syms)

# Replace the target list below with your own:
list(
  tar_target(pre_clean_files,
             preclean_files(files = origin_files),
             format = 'file'),
  tar_target(
    all_sentenced_dirty,
    sentencify(precleaned_files = pre_clean_files),
    format = 'file'
  ),
  tar_target(
    all_sentenced_dirty_lowercase,
    all_sentenced_dirty %>%
      files_lowercase(),
    format = 'file'
  ),
  tar_eval(
    tar_target(
      target_name,
      simple_sample(sample_target,
                    sample_size),
      format = 'file'
    ),
    values = simple_samples_pars %>% as.list()
  ),
  tar_eval(
    tar_target(name = split_name,
               create_our_splits(for_sample)),
    values = list(
      for_sample = simple_samples_pars$target_name,
      split_name = paste0("split_",
                          as.character(simple_samples_pars$target_name)) %>%
        syms()
    )
  ),
  tar_map(
    values = list(split_name=paste0("split_",
                    as.character(simple_samples_pars$target_name)) %>% syms()),
    tar_target(name = train_and_devtest,
               create_training_and_devset(split_name %>% rsample::training()))
  )
  # tar_target(
  #   testing,
  #   rsample::testing(split),
  #   pattern=map(splits)
  # ),
  # tar_target(
  #   training,
  #   train_and_devset %>% pluck('training'),
  #   pattern=map(train_and_devset)
  # ),
  # tar_target(
  #   devset,
  #   train_and_devset %>% pluck('devset'),
  #   pattern=map(train_and_devset)
  # )
  # )

)
