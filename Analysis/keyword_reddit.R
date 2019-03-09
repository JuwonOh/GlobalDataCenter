#########################
## SNU Global Data Center
## 2019 March
## Sooahn Shin & JONG HEE PARK
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

#########################
## user specific working directory setup
#########################
if(Sys.getenv("LOGNAME") == "park"){
    setwd("~/Dropbox/BigDataDiplomacy/Code/2019/Analysis")
    source("~/Github/Sooahn/GlobalDataCenter/Analysis/preprocess_functions.R")

}else{
    setwd("~/Dropbox/GlobalDataCenter/Analysis")
    source("preprocess_functions.R")
}

#########################
## Data
#########################
load("reddit_data.RData")
input_data <- reddit_data
month.name <- c("07" ,"08" ,"09" ,"10" ,"11" ,"12", "01", "02" )
file.name <- "~/Dropbox/BigDataDiplomacy/보고서/2019/plots/reddit"
subtitle = "2018.7 - 2019.3"
input = "Reddit"


#########################
## total frequency plot
#########################
## monthly count
monthly_n <- input_data %>%
  group_by(year, month) %>%
  count() %>%
    mutate(date = as.Date(paste0(year, month, "01"),"%Y%m%d")) %>%
    ungroup()



title = paste0("Korea-related ", input,  " Post Frequency") ; 
p0 <- ggplot(monthly_n) + 
    geom_bar(aes(x=date, y=n), stat="identity", alpha=0.2) +
    geom_line(aes(x=date, y=n), size=1) +
    geom_point(aes(x=date, y=n), alpha=0.6) +
    ## scale_shape_manual(values = c(1:length(unique(monthly_n$source)))) +
    scale_x_date(date_breaks = "months" , date_labels = "%Y-%b") + 
    labs(title=title, subtitle = subtitle, y = "Absolute Frequency", x="Month", 
         caption = "Copyright: SNU IIS Global Data Center")
pdf(file=paste0(file.name, "_totalfreq.pdf"),
    family="sans", width=12, height=8)
p0
dev.off()

#########################
## unigram keyword
#########################
input_data <- as_tibble(input_data)
input_unigrams <- input_data %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 1, collapse = F) %>%
  filter(!ngram %in% stop_words$word) %>%
  mutate(stemmed = wordStem(ngram))

input_unigrams_by_article <- input_unigrams %>%
  count(id_row, ngram) %>%
  bind_tf_idf(ngram, id_row, n) %>%
  arrange(desc(tf_idf)) %>%
  left_join(input_data[,c("id_row","year","month", "date")]) %>%
  group_by(year, month, ngram) %>%
  mutate(n_month = n(), tf_idf_month = sum(tf_idf)) %>%
  ungroup()

#########################
## bigram keyword
#########################
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
  group_by(year, month, ngram) %>%
  mutate(n_month = n(), tf_idf_month = sum(tf_idf)) %>%
    ungroup()


## keyword_execution
if(Sys.getenv("LOGNAME") == "park"){
    source("~/Github/Sooahn/GlobalDataCenter/Analysis/keyword_execution.R")

}else{
    source("keyword_execution.R")
}

q()
