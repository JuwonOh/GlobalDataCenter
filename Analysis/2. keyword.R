## SNU Global Data Center
## 2019 March
## Sooahn Shin
rm(list=ls())

library(tidyverse)
library(tidytext)
library(SnowballC)
library(udpipe)
library(lattice)
library(wesanderson)
setwd("~/Dropbox/GlobalDataCenter/Analysis")

load("thinktank_data.RData")
load("news_data.RData")

news_data <- news_data %>%
  mutate(text = paste(content, description, title, summary)) 

prep_fun <- function(x) {
  # make text lower case
  x = str_to_lower(x)
  
  ## drop all numbers and symbols 
  x <- gsub("[]$*?[^{|\\#%&~_/<=>!,:;`\")(}@-]", " ", x)
  x <- gsub("[(\\.)'’“”—…–•]", " ", x)
  x <- gsub("[0-9]", " ", x)
  
  # collapse multiple spaces
  x = str_replace_all(x, "\\s+", " ")
  
  return(x)
}

prep_fun2 <- function(x) {
  ## often-used multigram
  x <- gsub("( na )|( news app )|( fox news )|( new york times )|( wall street journal )", " ", x)
  x <- gsub("kim( )?jung( )?un|kim( )?jong( )?un", "kimjongun", x)
  x <- gsub("(leader kimjongun )|(leader kim )", "kimjongun ", x)
  
  x <- gsub("moon( )?jae( )?in", "moonjaein", x)
  x <- gsub("(president moonjaein )|(president moon )", "moonjaein ", x)
  
  x <- gsub("white house", "whitehouse", x)
  x <- gsub("trump administration", "trumpadministration", x)
  
  x <- gsub("human rights", "humanrights", x)
  x <- gsub("north korea", "northkorea", x)
  x <- gsub("south korea", "southkorea", x)
  
  x <- gsub("national security", "nationalsecurity", x)
  x <- gsub("trade war", "tradewar", x)
  x <- gsub("rex tillerson", "rextillerson", x)
  x <- gsub("winter olympic", "winterolympic", x)
  
  x <- gsub("nuclear weapon", "nuclearweapon", x)
  x <- gsub("nuclear threat", "nuclearthreat", x)
  x <- gsub("nuclear button", "nuclearbutton", x)
  x <- gsub("nuclear attack", "nuclearattack", x)
  
  x <- gsub("nobel peace prize", "nobelpeaceprize", x)
  x <- gsub("foreign policy", "foreignpolicy", x)
  
  x <- gsub("secretary (state )?pompeo", "pompeo", x)
  
  x <- gsub("soviet union", "sovietunion", x)
  x <- gsub("donald trump", "trump", x)
  x <- gsub("president trump", "trump", x)
  x <- gsub("barack obama", "obama", x)
  x <- gsub("president obama", "obama", x)
  
  x <- gsub("united nations", "unitednations", x)
  x <- gsub("middle east", "middleeast", x)
  x <- gsub("kim( )?yo( )?jong", "kimyojong", x)
  x <- gsub("vladimir putin", "vladimirputin", x)
  x <- gsub("president vladimirputin", "vladimirputin", x)
  
  ## remove stopwords
  stopwords_regex <- paste(tm::stopwords('en'),
                           collapse = '\\b|\\b')
  x <- str_replace_all(x, stopwords_regex, '')
  
  ## drop a single letter word...
  x <- gsub("\\s*(?<!\\S)[a-zA-Z]{1}(?!\\S)", " ", x, perl=T)
  
  # collapse multiple spaces
  x = str_replace_all(x, "\\s+", " ")
  
  return(x)
}

news_data$text = prep_fun(news_data$text)
news_data$text = prep_fun2(news_data$text)

save(news_data, file="~/Dropbox/GlobalDataCenter/Analysis/news_data.RData")

news_unigrams <- news_data %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 1) %>%
  filter(!ngram %in% stop_words$word) %>%
  mutate(stemmed = wordStem(ngram))

news_unigrams_by_article <- news_unigrams %>%
  count(id_row, ngram) %>%
  bind_tf_idf(ngram, id_row, n) %>%
  arrange(desc(tf_idf)) %>%
  left_join(news_data[,c("id_row","month")]) %>%
  group_by(month,ngram) %>%
  mutate(n_month = n(), tf_idf_month = sum(tf_idf)) %>%
  ungroup()

news_unigrams_by_article %>%
  select(ngram,month,n_month,tf_idf_month) %>%
  distinct() %>%
  group_by(month) %>%
  top_n(5, wt=n_month) %>%
  arrange(month,-n_month)

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
  left_join(news_data[,c("id_row","month")]) %>%
  group_by(month,ngram) %>%
  mutate(n_month = n(), tf_idf_month = sum(tf_idf)) %>%
  ungroup()

na_bigrams <- c("email sign", "content programming", "copies content",
                "copyright network", "copyright notice", "displayed published",
                "distributed transmitted", "american people", "alter remove",
                "final form", "llc rights", "network llc", "notice copies",
                "prior written", "programming copyright", "protected united",
                "published broadcast","remove trademark","reproduced distributed",
                "reserved copyright","rush transcript","trademark copyright",
                "transmitted displayed","written permission","copyright law",
                "washington post", "york city", "ap photo", "rights reserved",
                "july copy", "begin video", "cq roll", "copyright cq", "permission cq",
                "video clip")

test <- news_bigrams_by_article %>%
  select(ngram,month,n_month,tf_idf_month) %>%
  distinct() %>%
  filter(!ngram %in% na_bigrams) %>%
  group_by(month) %>%
  top_n(10, wt=n_month) %>%
  arrange(month,-n_month)

news_data$text[which(str_count(news_data$text,"moonjaein jae")!=0)[1]]
news_data$source[which(str_count(news_data$text,"jamal khashogg")!=0)]

