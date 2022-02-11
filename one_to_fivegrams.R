source('globals.R')
plan(future::multisession(workers = 4))
parallelly::supportsMulticore()
get_clean_splits(tag = 'initial') -> atrain


print("Loaded training set")


ngram_files  <- c()

c(1:5) %>%
  walk(function(x)    {
    print(glue::glue("starting on {x} grams"))
      r <- do_map_split_tokens(all_text = atrain,filename_tag = 'initial',ngram_size = x)
    print(glue::glue("Done"))
    ngram_files[x] <<- r
    gc(full=TRUE)
  }
)


print("Split our tokens to files:")
ngram_files

rm(atrain)
gc(full=TRUE)
outfiles <- c("onegram.csv","twogram.csv","threegram.csv","fourgram.csv","fivegram.csv") %>% paste0(out_data_dir,'/',.)

  seq_along(ngram_files) %>%
  future_walk(~do_model(file = ngram_files[.x]) %>% fwrite(.,file=outfiles[.x]))

  print("Did our models")
  print("they are in")
  print(outfiles)
