connect_to_ddb <- function(model_file){

  dbConnect(duckdb::duckdb(),
            dbdir=model_file)
}

write_dset <- function(dset,model_name,model_file){
  fs::file_delete(model_file)
  con <- connect_to_ddb(model_file)
  dbWriteTable(conn = con,
               name=model_name,
               value=dset,
               overwrite=TRUE)
  dbDisconnect(con)
}

executeQuery <- function(model_file,query){
  con <- dbConnect(duckdb(),dbdir=model_file)
  query %>%
    get_query() %>%
    r <- dbGetQuery(con,.)
    dbDisconnect(con)
    return(r)
}
