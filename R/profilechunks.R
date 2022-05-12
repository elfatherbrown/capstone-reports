devtest %>%
  group_by(sentence_id) %>%
  nest() %>%
  as_tibble() %>%
  pmap(function(sentence_id,data,...){
    evaluate_ngram(model_dt,data)
  }) %>%
  rbindlist()

devtest %>%
  group_by(sentence_id) %>%
  nest() %>%
  as_tibble() %>%
  future_pmap(function(sentence_id,data,...){
    evaluate_ngram(model_dt,data)
  }) %>%
  rbindlist()
