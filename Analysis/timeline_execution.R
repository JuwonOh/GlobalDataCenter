
if(input == "Reddit"){
    
#########################
## corpus prep
#########################
## unigram keyword
input_unigrams <- input_data %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 1, collapse = F) %>%
  filter(!ngram %in% stop_words$word) %>%
  mutate(stemmed = wordStem(ngram))

input_unigrams_by_article <- input_unigrams %>%
  count(id_row, ngram) %>%
  bind_tf_idf(ngram, id_row, n) %>%
  arrange(desc(tf_idf)) %>%
  left_join(input_data[,c("id_row","year","month", "date")]) %>%
  group_by(date,ngram) %>%
  mutate(n_date = n(), tf_idf_date = sum(tf_idf)) %>%
  group_by(year, month, ngram) %>%
  mutate(n_month = n(), tf_idf_month = sum(tf_idf)) %>%
  ungroup()

## bigram keyword
input_bigrams <- input_data %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 2, collapse = F) %>%
  separate(ngram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  unite(ngram, word1, word2, sep = " ")

input_bigrams_by_article <- input_bigrams %>%
    count(id_row, ngram) %>%
    bind_tf_idf(ngram, id_row, n) %>%
    arrange(desc(tf_idf)) %>%
    left_join(input_data[,c("id_row","year","month", "date")]) %>%
    group_by(date,ngram) %>%
    mutate(n_date = n(), tf_idf_date = sum(tf_idf)) %>%
    group_by(year, month, ngram) %>%
    mutate(n_month = n(), tf_idf_month = sum(tf_idf)) %>%
    ungroup()

}else{
#########################
## corpus prep
#########################
## unigram keyword
input_unigrams <- input_data %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 1) %>%
  filter(!ngram %in% stop_words$word) %>%
  mutate(stemmed = wordStem(ngram))

input_unigrams_by_article <- input_unigrams %>%
  count(id_row, ngram) %>%
  bind_tf_idf(ngram, id_row, n) %>%
  arrange(desc(tf_idf)) %>%
  left_join(input_data[,c("id_row","year","month", "date")]) %>%
  group_by(date,ngram) %>%
  mutate(n_date = n(), tf_idf_date = sum(tf_idf)) %>%
  group_by(year, month, ngram) %>%
  mutate(n_month = n(), tf_idf_month = sum(tf_idf)) %>%
  ungroup()

## bigram keyword
input_bigrams <- input_data %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 2) %>%
  separate(ngram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  unite(ngram, word1, word2, sep = " ") 

input_bigrams_by_article <- input_bigrams %>%
    count(id_row, ngram) %>%
    bind_tf_idf(ngram, id_row, n) %>%
    arrange(desc(tf_idf)) %>%
    left_join(input_data[,c("id_row","year","month", "date")]) %>%
    group_by(date,ngram) %>%
    mutate(n_date = n(), tf_idf_date = sum(tf_idf)) %>%
    group_by(year, month, ngram) %>%
    mutate(n_month = n(), tf_idf_month = sum(tf_idf)) %>%
    ungroup()
}

df.all <- rbind(input_unigrams_by_article, input_bigrams_by_article) 
df.unigram <- input_unigrams_by_article
df.bigram <- input_bigrams_by_article
df.bigram$date <- as.Date(paste(df.bigram$date,"-01",sep=""))



#########################
## plot
#########################
## leaders
word.list <- c("moonjaein", "jinping", "abe | abe", "kimjungun")
if(Sys.getenv("LOGNAME") == "park"){
    source("~/Github/Sooahn/GlobalDataCenter/Analysis/timelineGraph.R")

}else{
    source("timelineGraph.R")
}

## iran is outstanding!
word.list <- c("venezuela", "iran", "libya", "iraq")
if(Sys.getenv("LOGNAME") == "park"){
    source("~/Github/Sooahn/GlobalDataCenter/Analysis/timelineGraph.R")

}else{
    source("timelineGraph.R")
}

## nuclear weapons is outstanding!
word.list <- c("denuclearization", "missile", "uranium", "plutonium")
if(Sys.getenv("LOGNAME") == "park"){
    source("~/Github/Sooahn/GlobalDataCenter/Analysis/timelineGraph.R")

}else{
    source("timelineGraph.R")
}


## nuclear site
word.list <- c("yongbyon", "sino | sino", "hidden site", "ballistic missile")
if(Sys.getenv("LOGNAME") == "park"){
    source("~/Github/Sooahn/GlobalDataCenter/Analysis/timelineGraph.R")

}else{
    source("timelineGraph.R")
}
