#########################
## SNU Global Data Center
## 2019 March
## Sooahn Shin
#########################
rm(list=ls())

library(tidyverse)
library(tidytext)
library(SnowballC)
library(udpipe)
library(lattice)
library(wesanderson)

## user specific working directory setup

if(Sys.getenv("LOGNAME") == "park"){
    setwd("~/Dropbox/BigDataDiplomacy/Code/2019/Analysis")
}else{
    setwd("~/Dropbox/GlobalDataCenter/Analysis")
}

### News Data
source("preprocess_functions.R")
load("news_data.RData")


## monthly count
monthly_n <- news_data %>%
  group_by(year, month, source) %>%
  count() %>%
    mutate(date = as.Date(paste0(year, month, "01"),"%Y%m%d")) %>%
    ungroup

monthly_n <- monthly_n %>% mutate(source = recode(source, fox = "Fox",
                                                  wsj = "Wall Street Journal",
                                                  nyt = "New York Times"
                                                  ))
           
title = "Korea-related Article Frequency" ; subtitle = "2018.7 - 2019.3"
ggplot(monthly_n) + 
    geom_bar(aes(x=date, y=n), stat="identity", alpha=0.5) +
    geom_line(aes(x=date, y=n, col=source), size = 3, alpha=0.5) + 
    labs(title=title, subtitle = subtitle, y = "Frequency", x="Month", 
         caption = "Copyright: SNU IIS Global Data Center")


## unigram keyword
# news_unigrams <- news_data %>%
#   unnest_tokens(ngram, text, token = "ngrams", n = 1) %>%
#   filter(!ngram %in% stop_words$word) %>%
#   mutate(stemmed = wordStem(ngram))
# 
# news_unigrams_by_article <- news_unigrams %>%
#   count(id_row, ngram) %>%
#   bind_tf_idf(ngram, id_row, n) %>%
#   arrange(desc(tf_idf)) %>%
#   left_join(news_data[,c("id_row","month")]) %>%
#   group_by(month,ngram) %>%
#   mutate(n_month = n(), tf_idf_month = sum(tf_idf)) %>%
#   ungroup()
# 
# news_unigrams_by_article %>%
#   select(ngram,month,n_month,tf_idf_month) %>%
#   distinct() %>%
#   group_by(month) %>%
#   top_n(5, wt=n_month) %>%
#   arrange(month,-n_month)

## bigram keyword

news_bigrams <- news_data %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 2) %>%
  separate(ngram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  unite(ngram, word1, word2, sep = " ")

news_bigrams_by_article <- news_bigrams %>%
  count(id_row, ngram) %>%
  bind_tf_idf(ngram, id_row, n) %>%
  arrange(desc(tf_idf)) %>%
  left_join(news_data[,c("id_row","year","month")]) %>%
  group_by(year, month, ngram) %>%
  mutate(n_month = n(), tf_idf_month = sum(tf_idf)) %>%
  ungroup()

top10 <- news_bigrams_by_article %>%
  select(ngram,year,month,n_month,tf_idf_month) %>%
  distinct() %>%
  filter(!ngram %in% na_bigrams) %>%
  group_by(year,month) %>%
  top_n(10, wt=n_month) %>%
  arrange(year,month,-n_month)

# news_data$text[which(str_count(news_data$text,"moonjaein jae")!=0)[1]]

