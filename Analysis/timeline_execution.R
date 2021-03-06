collapse.input <- ifelse(input == "Reddit",F,T)
    
#########################
## corpus prep
#########################
## unigram keyword
input_unigrams <- input_data %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 1, collapse = collapse.input) %>%
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
  unnest_tokens(ngram, text, token = "ngrams", n = 2, collapse = collapse.input) %>%
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

df.all <- rbind(input_unigrams_by_article, input_bigrams_by_article) 
df.unigram <- input_unigrams_by_article
df.bigram <- input_bigrams_by_article
df.bigram$date <- as.Date(paste(df.bigram$date,"-01",sep=""))



#########################
## plot
#########################
## iran venezuela
word.list <- c("iran", "venezuela", "northkorea")
word.sentiment <- NA
if(Sys.getenv("LOGNAME") == "park"){
  source("~/Github/Sooahn/GlobalDataCenter/Analysis/timelineGraph_jhp.R")
  
}else{
  source("timelineGraph_jhp.R")
}

## security
word.list <- c("peace", "assistance", ## "appease", 
               "weapon", "missile", "rocket")
word.sentiment <- NA

## word.sentiment <- c("positive","positive", ## "positive", "negative","negative","negative")
if(Sys.getenv("LOGNAME") == "park"){
  source("~/Github/Sooahn/GlobalDataCenter/Analysis/timelineGraph_jhp.R")
  
}else{
  source("timelineGraph_jhp.R")
}


## economy
word.list <- c("indo pacific", "midterm election", "price energy", 
               "free trade", "international norms", "economic freedom")
word.sentiment <- NA
## word.sentiment <- c("positive","positive", "positive",
##                     "neutral", "neutral", "neutral")
if(Sys.getenv("LOGNAME") == "park"){
  source("~/Github/Sooahn/GlobalDataCenter/Analysis/timelineGraph_jhp.R")
  
}else{
  source("timelineGraph_jhp.R")
}
## economy
word.list <- c("indo pacific", "energy", "protection", 
               "trade", "norm", "freedom")
word.sentiment <- NA
## word.sentiment <- c("positive","positive", "positive",
##                     "neutral", "neutral", "neutral")
if(Sys.getenv("LOGNAME") == "park"){
  source("~/Github/Sooahn/GlobalDataCenter/Analysis/timelineGraph_jhp.R")
  
}else{
  source("timelineGraph_jhp.R")
}



## 높아지는 미국의 요구
## word.list <- c("hidden site", "appeasement",
##                "human right", "sunset clause")
## word.sentiment <- c("negative","negative","negative","neutral",
##                     "negative","negative","neutral")
## if(Sys.getenv("LOGNAME") == "park"){
##   source("~/Github/Sooahn/GlobalDataCenter/Analysis/timelineGraph_jhp.R")
##   
## }else{
##   source("timelineGraph_jhp.R")
## }

## small deal
## word.list <- c("peace talk", "peace process", "sanction lift", "economic assistance",
##                "plutonium", "nuclear test")
## word.sentiment <- c("positive","positive","positive","positive",
##                     "negative","negative")
## if(Sys.getenv("LOGNAME") == "park"){
##   source("~/Github/Sooahn/GlobalDataCenter/Analysis/timelineGraph_jhp.R")
##   
## }else{
##   source("timelineGraph_jhp.R")
## }

## 높아지는 미국의 요구
## word.list <- c("missile", "rocket", "hidden site", "appeasement",
##                "chemical weapon", "human right", "sunset clause")
## word.sentiment <- c("negative","negative","negative","neutral",
##                     "negative","negative","neutral")
## if(Sys.getenv("LOGNAME") == "park"){
##   source("~/Github/Sooahn/GlobalDataCenter/Analysis/timelineGraph_jhp.R")
  
## }else{
##   source("timelineGraph_jhp.R")
## }


# ## leaders
# word.list <- c("moonjaein", "jinping", "abe", "kimjongun")
# word.sentiment <- c("neutral","neutral","neutral","neutral")
# if(Sys.getenv("LOGNAME") == "park"){
#     source("~/Github/Sooahn/GlobalDataCenter/Analysis/timelineGraph_jhp.R")
# 
# }else{
#     source("timelineGraph_jhp.R")
# }
# 
# ## iran is outstanding!
# word.list <- c("venezuela", "iran", "libya", "iraq")
# word.sentiment <- c("neutral","neutral","neutral","neutral")
# if(Sys.getenv("LOGNAME") == "park"){
#     source("~/Github/Sooahn/GlobalDataCenter/Analysis/timelineGraph_jhp.R")
# 
# }else{
#     source("timelineGraph_jhp.R")
# }
# 
# ## nuclear weapons is outstanding!
# word.list <- c("denuclearization", "missile", "uranium", "plutonium")
# word.sentiment <- c("positive","negative","negative","negative")
# if(Sys.getenv("LOGNAME") == "park"){
#     source("~/Github/Sooahn/GlobalDataCenter/Analysis/timelineGraph_jhp.R")
# 
# }else{
#     source("timelineGraph_jhp.R")
# }
# 
# 
# ## nuclear site
# word.list <- c("yongbyon", "sino", "hidden site", "ballistic missile")
# word.sentiment <- c("negative","negative","negative","negative")
# if(Sys.getenv("LOGNAME") == "park"){
#     source("~/Github/Sooahn/GlobalDataCenter/Analysis/timelineGraph_jhp.R")
# 
# }else{
#     source("timelineGraph_jhp.R")
# }
