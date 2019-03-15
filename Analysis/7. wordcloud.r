#########################
## SNU Global Data Center
## 2019 March
## Juwon Oh
#########################
rm(list=ls())

#########################
## package loading
#########################
library(tidyverse)
library(tidytext)
library(SnowballC)
library(udpipe)
library(lattice)
library(wesanderson)
library(RColorBrewer)
library(wordcloud)

#########################
## user specific working directory setup
#########################
if(Sys.getenv("LOGNAME") == "park"){
    setwd("~/Dropbox/BigDataDiplomacy/Code/2019/Analysis")
    source("~/Github/Sooahn/GlobalDataCenter/Analysis/preprocess_functions.R")

  
  
}else{
    setwd("~/github/GlobalDataCenter/Analysis")
    source("preprocess_functions.R")
}

#########################
## Data
#########################
load("reddit_data.RData")
input_data <- reddit_data
month.name <- c("07" ,"08" ,"09" ,"10" ,"11" ,"12", "01", "02" )
file.name <- "~/Dropbox/BigDataDiplomacy/보고서/2019/plots/reddit_timeline"
subtitle = "2018.7 - 2019.3"
input = "News"
pre_midturm = c("07" ,"08" ,"09" ,"10")
post_midturm = c("11" ,"12", "01", "02")

#########################
## unigram keyword
#########################

input_data <- as_tibble(input_data)
input_unigrams <- input_data %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 1, collapse = F) %>%
  filter(!ngram %in% stop_words$word) %>%
  mutate(stemmed = wordStem(ngram))

pre_midturm <- input_unigrams%>% 
  filter(month == pre_midturm)

pre_midturm_frequency <- count(pre_midturm, ngram, sort = TRUE)%>% 
  filter(!ngram %in% c("china","northkorea", "japan", "russia", "trump", "world", "united", "president", "southkorea", "korean",
                       "chinese", "american", "russian", "kyrgyzstan", "korea", "nkkorea", "northkorean", 've', 'll', 'don',
                       'people', 'kim', 'kimjongun','america'))

post_midturm <- input_unigrams%>% 
  filter(month == post_midturm)

post_midturm_frequency <- count(post_midturm, ngram, sort = TRUE)%>%
  filter(!ngram %in% c("china","northkorea", "japan", "russia", "trump", "world", "united", "president", "southkorea", "korean",
                       "chinese", "american", "russian", "kyrgyzstan", "korea", "nkkorea", "northkorean", 've', 'll', 'don',
                       'people', 'kim', 'kimjongun', 'america'))
#########################
## wordcloud
#########################

# pre election wordcloud
pal <- brewer.pal(12,"Paired")[-1]
png("reddit_pre-election.png", width = 200, height = 180, units='mm', res=400)
wordcloud = pre_midturm_frequency %>% 
  with(wordcloud(ngram, n, max.words = 100, random.order=FALSE, rot.per=.15, colors=pal))
dev.off()

# post election wordcloud
pal <- brewer.pal(12,"Paired")[-1]
png("reddit_post-election.png", width = 200, height = 180, units='mm', res=400)
wordcloud = post_midturm_frequency %>% 
  with(wordcloud(ngram, n, max.words= 100,min.freq=1, random.order=FALSE, rot.per=.15, colors=pal))
dev.off()

