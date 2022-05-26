parse_to_vec <- function(tt,max_order=4){
  words <- tt %>%
    str_split(" ") %>%
    pluck(1)
  wlen <- length(words)
  d <- wlen-max_order+1
  if(d>0){
    return(words[-(1:(d))])
  } else {
      return(words)
    }
}

match_table <- function(pvec){
  map2_dfr(1:length(pvec),
           seq.int(length(pvec),1),
          ~tibble(tx=paste(pvec[.x:length(pvec)],collapse=" "),
                  l=.y+1)
  )
    }

get_where <- function(pvec){
  map2_chr(1:length(pvec),
           seq.int(length(pvec),1),
           function(x,y){
             m <-  paste0(pvec[x:length(pvec)],collapse=" ") %>%
               paste0(" ([^ ]+)$")
             glue("(regexp_matches(n_gram,'(^|<s>){m}') AND \"order\"={y+1})")

           }) %>%
    paste0(collapse=' OR ')
}

get_query <- function(thestr){
  w <- thestr %>%
    parse_to_vec() %>%
    get_where()
  glue("SELECT * FROM(SELECT n_gram,\"order\",prob,backoff,rank() OVER (PARTITION BY \"order\" ORDER BY prob DESC)",
       "from chosen_model where {w} ) ",
       "WHERE rank<5",
       sep='\n')
  }

