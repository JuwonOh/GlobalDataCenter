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
library(gridExtra)

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
load("news_data.RData")
input_data <- news_data
month.name <- c("07" ,"08" ,"09" ,"10" ,"11" ,"12", "01", "02" )
file.name <- "~/Dropbox/BigDataDiplomacy/보고서/2019/plots/timeline/news"
subtitle = "2018.7 - 2019.3"
input = "News"


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

df.all <- rbind(input_unigrams_by_article, input_bigrams_by_article) 
df.unigram <- input_unigrams_by_article
df.bigram <- input_bigrams_by_article
#########################
## plot
#########################
## leaders
word.list <- c("moonjaein", "jinping", "abe | abe", "kimjungun")
p.list = lapply(1:length(word.list), function(i) {
    df.bigram %>% filter(str_detect(ngram, word.list[i]))%>%
        ## filter(ngram%in% word.list[[i]]) %>%
        select(date, n_month, ngram) %>%
        distinct() %>%
        ggplot(., aes(x=as.Date(date), y=n_month, color=ngram), group=ngram) + 
        geom_point(size=2, alpha=0.9) +
        scale_x_date(date_minor_breaks = "1 day", date_labels = "%Y-%m") +
        xlab("Month") + ylab("Absolute Frequency") +
        theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) + 
        theme_minimal() +
        theme(legend.key=element_blank(), legend.key.size=unit(0.5,"point")) +
        guides(colour=guide_legend(ncol=12)) + 
        theme(legend.title = element_blank(),legend.text=element_text(size=rel(0.5)),
              legend.position="bottom") +
        labs(caption = "Copyright: SNU IIS Global Data Center")
})
for(i in 1:length(word.list)){
    pdf(file=paste0(file.name, "_", word.list[[i]], "_bigram.pdf"), family="sans",
        width = 12, height = 7)
    print(p.list[[i]])
    dev.off()
}

## iran is outstanding!
word.list <- c("venezuela", "iran", "libya", "iraq")
p.list = lapply(1:length(word.list), function(i) {
    df.bigram %>% filter(str_detect(ngram, word.list[i]))%>%
        ## filter(ngram%in% word.list[[i]]) %>%
        select(date, n_month, ngram) %>%
        distinct() %>%
        ggplot(., aes(x=as.Date(date), y=n_month, color=ngram), group=ngram) + 
        geom_point(size=2, alpha=0.9) +
        scale_x_date(date_minor_breaks = "1 day", date_labels = "%Y-%m") +
        xlab("Month") + ylab("Absolute Frequency") +
        theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) + 
        theme_minimal() +
        theme(legend.key=element_blank(), legend.key.size=unit(0.5,"point")) +
        guides(colour=guide_legend(ncol=12)) + 
        theme(legend.title = element_blank(),legend.text=element_text(size=rel(0.5)),
              legend.position="bottom") +
        labs(caption = "Copyright: SNU IIS Global Data Center")
})
for(i in 1:length(word.list)){
    pdf(file=paste0(file.name, "_", word.list[[i]], "_bigram.pdf"), family="sans",
        width = 12, height = 10)
    print(p.list[[i]])
    dev.off()
}

## nuclear weapons is outstanding!
word.list <- c("denuclearization", "missile", "uranium", "plutonium")
p.list = lapply(1:length(word.list), function(i) {
    df.bigram %>% filter(str_detect(ngram, word.list[i]))%>%
        ## filter(ngram%in% word.list[[i]]) %>%
        select(date, n_month, ngram) %>%
        distinct() %>%
        ggplot(., aes(x=as.Date(date), y=n_month, color=ngram), group=ngram) + 
        geom_point(size=2, alpha=0.9) +
        scale_x_date(date_minor_breaks = "1 day", date_labels = "%Y-%m") +
        xlab("Month") + ylab("Absolute Frequency") +
        theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) + 
        theme_minimal() +
        theme(legend.key=element_blank(), legend.key.size=unit(0.5,"point")) +
        guides(colour=guide_legend(ncol=12)) + 
        theme(legend.title = element_blank(),legend.text=element_text(size=rel(0.5)),
              legend.position="bottom") +
        labs(caption = "Copyright: SNU IIS Global Data Center")
})
for(i in 1:length(word.list)){
    pdf(file=paste0(file.name, "_", word.list[[i]], "_bigram.pdf"), family="sans",
        width = 16, height = 10)
    print(p.list[[i]])
    dev.off()
}


## nuclear site
word.list <- c("yongbyon", "sino | sino", "hidden site", "ballistic missile")
p.list = lapply(1:length(word.list), function(i) {
    df.bigram %>% filter(str_detect(ngram, word.list[i]))%>%
        ## filter(ngram%in% word.list[[i]]) %>%
        select(date, n_month, ngram) %>%
        distinct() %>%
        ggplot(., aes(x=as.Date(date), y=n_month, color=ngram), group=ngram) + 
        geom_point(size=2, alpha=0.9) +
        scale_x_date(date_minor_breaks = "1 day", date_labels = "%Y-%m") +
        xlab("Month") + ylab("Absolute Frequency") +
        theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) + 
        theme_minimal() +
        theme(legend.key=element_blank(), legend.key.size=unit(0.5,"point")) +
        guides(colour=guide_legend(ncol=12)) + 
        theme(legend.title = element_blank(),legend.text=element_text(size=rel(0.5)),
              legend.position="bottom") +
        labs(caption = "Copyright: SNU IIS Global Data Center")
})
for(i in 1:length(word.list)){
    pdf(file=paste0(file.name, "_", word.list[[i]], "_bigram.pdf"), family="sans",
        width = 12, height = 8)
    print(p.list[[i]])
    dev.off()
}

