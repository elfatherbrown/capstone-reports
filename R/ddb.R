con_model_file <- function(model_file){
  dbConnect(duckdb::duckdb(),
            dbdir=model_file)
}

write_dset <- function(con,dset,model_name){
  dbWriteTable(conn = con,
               name=model_name,
               value=dset,
               overwrite=TRUE)
}

executeQuery <- function(model_file,query){
  con <- dbConnect(duckdb(),dbdir=model_file)
  query %>%
    get_query() %>%
    r <- dbGetQuery(con,.)
    dbDisconnect(con)
    return(r)
}
