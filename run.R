# This is a helper script to run the pipeline.
# Choose how to execute the pipeline below.
# See https://books.ropensci.org/targets/hpc.html
# to learn about your options.

targets::tar_make(c("evaluate_models",
                    "consolidated_evaluations",
                    "chosen_language_model",
                    "models_as_database_file",
                    "main_table_name",
                    "model_as_database_file"))
# targets::tar_make_clustermq(workers = 2) # nolint
# targets::tar_make_future(workers = 2) # nolint
