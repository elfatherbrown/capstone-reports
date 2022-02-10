source('globals.R')
plan(future::multisession(workers = 4))
parallelly::supportsMulticore()
get_clean_splits() %>%
  training(.) -> atrain


print("Loaded training set")


ngram_files <- c(3,4,5) %>%
  map_chr(~ do_map_split_tokens(atrain,ngram_size = .x))


print("Split our tokens")


rm(atrain)
gc(full=TRUE)
outfiles <- c("threegram.csv","fourgram.csv","fivegram.csv") %>% paste0(out_data_dir,'/',.)

  seq_along(ngram_files) %>%
  walk(~do_model(file = ngram_files[.x]) %>% fwrite(.,file=outfiles[.x]))

  print("Did our models")
  print("they are in")
  print(outfiles)
