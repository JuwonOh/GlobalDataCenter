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
    source("~/Github/Sooahn/GlobalDataCenter/Analysis/preprocess_functions.R")

}else{
    setwd("~/Dropbox/GlobalDataCenter/Analysis")
    source("preprocess_functions.R")
}

### News Data
load("thinktank_data.RData")
input_data <- thinktank_data
month.name <- c("07" ,"08" ,"09" ,"10" ,"11" ,"12", "01", "02" )
file.name <- "~/Dropbox/BigDataDiplomacy/보고서/2019/plots/thinktank"
subtitle = "2018.7 - 2019.3"
input = "Thinktank"

#########################
## total frequency plot
#########################
## monthly count
monthly_n <- input_data %>%
  group_by(year, month, source) %>%
  count() %>%
    mutate(date = as.Date(paste0(year, month, "01"),"%Y%m%d")) %>%
    ungroup()

## %>% mutate(source = recode(source, fox = "Fox",
##                                         wsj = "Wall Street Journal",
##                                         nyt = "New York Times"
##                                         ))
title = paste0("Korea-related ", input,  " Article Frequency") ; 
p0 <- ggplot(monthly_n) + 
    geom_bar(aes(x=date, y=n), stat="identity", alpha=0.5) +
    geom_line(aes(x=date, y=n, col=source), size = 3, alpha=0.5) + 
    labs(title=title, subtitle = subtitle, y = "Frequency", x="Month", 
         caption = "Copyright: SNU IIS Global Data Center")
pdf(file=paste0(file.name, "_totalfreq.pdf"),
    family="sans", width=12, height=8)
p0
dev.off()

#########################
## unigram keyword
#########################
input_unigrams <- input_data %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 1) %>%
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
  group_by(year, month, ngram) %>%
  mutate(n_month = n(), tf_idf_month = sum(tf_idf)) %>%
    ungroup()

#########################
## top 40 uniigram plot
#########################
top40 <- input_unigrams_by_article %>%
    select(ngram,year,month,n_month,tf_idf_month) %>%
    distinct() %>%
    filter(!ngram %in% na_bigrams) %>%
    group_by(year,month) %>%
    top_n(40, wt=tf_idf_month) %>%
    arrange(year,month,-tf_idf_month) %>%
    mutate(time = paste(year, month, sep="-"))

## because of facetting, ordering is not working.
## So, we have to ungroup it and name a new variable "order"
pd0 <- top40 %>%
    group_by(month) %>%
    ## slice(10:n()) %>% ## excluding top 10 rows
    ungroup() %>%
    ## 2. Arrange by
    ##   i.  facet group
    ##   ii. bar height
    arrange(month, tf_idf_month) %>%
    ## 3. Add order column of row numbers
    mutate(order = row_number())

title = paste0("Unigram Word Frequency in Korea-related ", input, " Articles")

p01 <- ggplot(pd0, aes(order, tf_idf_month, fill = factor(month))) +
    ## geom_bar(stat = "identity", show.legend = FALSE) +
    ## Free the scales here
    geom_col(show.legend = FALSE) +
    facet_wrap(~ time, ncol = 4, scales = "free") +
    labs(title=title, subtitle = subtitle, y = "Relative Frequency", x="Month", 
         caption = "Copyright: SNU IIS Global Data Center") +
    ## Add categories to axis
    scale_x_continuous(
        breaks = pd0$order,
        labels = pd0$ngram,
        expand = c(0,0)
    ) +
    coord_flip()
pdf(file=paste0(file.name, "_top40uni_relative.pdf"),
    family="sans", width=12, height=15)
p01
dev.off()


#########################
## top 20 bigram plot
#########################
top40 <- input_bigrams_by_article %>%
    select(ngram,year,month,n_month,tf_idf_month) %>%
    distinct() %>%
    filter(!ngram %in% na_bigrams) %>%
    group_by(year,month) %>%
    top_n(20, wt=n_month) %>%
    arrange(year,month,-n_month) %>%
    mutate(time = paste(year, month, sep="-"))

## because of facetting, ordering is not working.
## So, we have to ungroup it and name a new variable "order"
pd <- top20 %>%
    group_by(month) %>%
    ungroup() %>%
    ## 2. Arrange by
    ##   i.  facet group
    ##   ii. bar height
    arrange(month, n_month) %>%
    ## 3. Add order column of row numbers
    mutate(order = row_number())

title = paste0("Bigram Word Frequency in Korea-related ", input, " Articles")
p1 <- ggplot(pd, aes(order, n_month, fill = factor(month))) +
    ## geom_bar(stat = "identity", show.legend = FALSE) +
    ## Free the scales here
    geom_col(show.legend = FALSE) +
    facet_wrap(~ time, ncol = 4, scales = "free") +
    labs(title=title, subtitle = subtitle, y = "Frequency", x="Month", 
         caption = "Copyright: SNU IIS Global Data Center") +
    ## Add categories to axis
    scale_x_continuous(
        breaks = pd$order,
        labels = pd$ngram,
        expand = c(0,0)
    ) +
    coord_flip()
pdf(file=paste0(file.name, "_top20bigram.pdf"),
    family="sans", width=12, height=8)
p1
dev.off()

#########################
### top 40 bigram relative frequency
#########################
top40 <- input_bigrams_by_article %>%
    select(ngram,year,month,n_month,tf_idf_month) %>%
    distinct() %>%
    filter(!ngram %in% na_bigrams) %>%
    group_by(year,month) %>%
    top_n(40, wt=tf_idf_month) %>%
    arrange(year,month,-tf_idf_month) %>%
    mutate(time = paste(year, month, sep="-"))

## because of facetting, ordering is not working.
## So, we have to ungroup it and name a new variable "order"
pd <- top40 %>%
    group_by(month) %>%
    ungroup() %>%
    ## 2. Arrange by
    ##   i.  facet group
    ##   ii. bar height
    arrange(month, tf_idf_month) %>%
    ## 3. Add order column of row numbers
    mutate(order = row_number())


p11 <- ggplot(pd, aes(order, tf_idf_month, fill = factor(month))) +
    ## geom_bar(stat = "identity", show.legend = FALSE) +
    ## Free the scales here
    geom_col(show.legend = FALSE) +
    facet_wrap(~ time, ncol = 4, scales = "free") +
    labs(title=title, subtitle = subtitle, y = "Frequency", x="Month", 
         caption = "Copyright: SNU IIS Global Data Center") +
    ## Add categories to axis
    scale_x_continuous(
        breaks = pd$order,
        labels = pd$ngram,
        expand = c(0,0)
    ) +
    coord_flip()


title = paste0("Bigram Word Frequency in Korea-related ", input, " Articles") ;
## p.list = lapply(sort(unique(pd$time)), function(i) {
p12 <- ggplot(pd, aes(order, tf_idf_month, fill = factor(month))) +
    ## geom_bar(stat = "identity", show.legend = FALSE) +
    ## Free the scales here
    geom_col(show.legend = FALSE) +
    facet_wrap(~ time, ncol = 4, scales = "free") +
    labs(title=title, subtitle = subtitle, y = "Frequency", x="Bigram Word", 
         caption = "Copyright: SNU IIS Global Data Center") +
    ## Add categories to axis
    scale_x_continuous(
        breaks = pd$order,
        labels = pd$ngram,
        expand = c(0,0)
    ) +
        coord_flip()


## library(gridExtra)
pdf(file=paste0(file.name, "_top40relative_bigram.pdf"),
    family="sans", width=16, height=15)
p12 ## do.call(grid.arrange, c(p.list, nrow=2))
dev.off()

