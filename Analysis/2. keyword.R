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
load("news_data.RData")


#########################
## total frequency plot
#########################
## monthly count
monthly_n <- news_data %>%
  group_by(year, month, source) %>%
  count() %>%
    mutate(date = as.Date(paste0(year, month, "01"),"%Y%m%d")) %>%
    ungroup() %>% mutate(source = recode(source, fox = "Fox",
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
news_unigrams <- news_data %>%
  unnest_tokens(ngram, text, token = "ngrams", n = 1) %>%
  filter(!ngram %in% stop_words$word) %>%
  mutate(stemmed = wordStem(ngram))

news_unigrams_by_article <- news_unigrams %>%
  count(id_row, ngram) %>%
  bind_tf_idf(ngram, id_row, n) %>%
  arrange(desc(tf_idf)) %>%
  left_join(news_data[,c("id_row","year","month", "date")]) %>%
  group_by(year, month, ngram) %>%
  mutate(n_month = n(), tf_idf_month = sum(tf_idf)) %>%
  ungroup()

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
  left_join(news_data[,c("id_row","year","month", "date")]) %>%
  group_by(year, month, ngram) %>%
  mutate(n_month = n(), tf_idf_month = sum(tf_idf)) %>%
  ungroup()

#########################
## top 20 bigram plot
#########################
top20 <- news_bigrams_by_article %>%
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

title = "Bigram Word Frequency in Korea-related Articles" ; subtitle = "2018.7 - 2019.3"
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
pdf(file="~/Dropbox/BigDataDiplomacy/보고서/2019/plots/top20.pdf",
    family="sans", width=12, height=8)
p1
dev.off()

#########################
## timeline plot
#########################

news_unigrams_by_article <- news_unigrams_by_article %>%
  group_by(date,ngram) %>%
  mutate(n_date = n(), tf_idf_date = sum(tf_idf)) %>%
  ungroup()

news_bigrams_by_article <- news_bigrams_by_article %>%
  group_by(date,ngram) %>%
  mutate(n_date = n(), tf_idf_date = sum(tf_idf)) %>%
  ungroup()

##############################################
###----------  Plot A : 긴장국면 ----------###
##############################################
a <- rbind(news_unigrams_by_article, news_bigrams_by_article) %>%
  filter(ngram%in%c("bloody nose", "maximum pressure", "preemptive strike")) %>%
  mutate(ngram = recode(ngram, "bloody nose" = "Bloody Nose     ",
         "maximum pressure" = "Maximum Pressure     ",
         "preemptive strike" = "Preemptive Strike     "))

# !!!NOTE THAT!!!
# relative frequency = tf_idf_date
a %>%
  select(date, tf_idf_date, ngram) %>%
  distinct() %>%
  ggplot(., aes(x=date, y=tf_idf_date, group=ngram, color=ngram, shape=ngram)) + 
  geom_point(size=1, alpha=0.6) +
  stat_smooth(geom="line", size=1, alpha=0.5, method = 'lm', formula = y ~ poly(x,2), se= FALSE) +
  scale_x_date(date_minor_breaks = "1 day") + xlab("") + ylab("relative frequency") +
  scale_color_manual(values = wes_palette(n=3, name="Darjeeling1")) +
  theme_minimal() + theme(legend.title = element_blank(),legend.position="top") +
  labs(caption = "Copyright: SNU IIS Global Data Center")


##############################################
###----------  Plot B : 대화국면 ----------###
##############################################
b <- rbind(news_unigrams_by_article, news_bigrams_by_article) %>%
  filter(ngram%in%c("korea summit", "panmunjom", "peace treaty")) %>%
  mutate(ngram = recode(ngram, "korea summit" = "Korea Summit     ",
                        "panmunjom" = "Panmunjom     ",
                        "peace treaty" = "Peace Treaty     "))

b %>%
  select(date, tf_idf_date, ngram) %>%
  distinct() %>%
  ggplot(., aes(x=date, y=tf_idf_date, group=ngram, color=ngram, shape=ngram)) + 
  geom_point(size=1, alpha=0.6) +
  stat_smooth(geom="line", size=1, alpha=0.5, method = 'lm', formula = y ~ poly(x,2), se= FALSE) +
  scale_x_date(date_minor_breaks = "1 day") + xlab("") + ylab("relative frequency") +
  scale_color_manual(values = wes_palette(n=3, name="Darjeeling1")) +
  theme_minimal() + theme(legend.title = element_blank(),legend.position="top") +
  labs(caption = "Copyright: SNU IIS Global Data Center")

##############################################
###----------    Plot C : 북핵   ----------###
##############################################
c <- rbind(news_unigrams_by_article, news_bigrams_by_article) %>%
  filter(ngram%in%c("northkorea iran", "nuclear test", "nuclearweapon")) %>%
  mutate(ngram = recode(ngram, "northkorea iran" = "North Korea Iran     ",
                        "nuclear test" = "Nuclear Test     ",
                        "nuclearweapon" = "Nuclear Weapon     "))

c %>%
  select(date, tf_idf_date, ngram) %>%
  distinct() %>%
  ggplot(., aes(x=date, y=tf_idf_date, group=ngram, color=ngram, shape=ngram)) + 
  geom_point(size=1, alpha=0.6) +
  stat_smooth(geom="line", size=1, alpha=0.5, method = 'lm', formula = y ~ poly(x,2), se= FALSE) +
  scale_x_date(date_minor_breaks = "1 day") + xlab("") + ylab("relative frequency") +
  scale_color_manual(values = wes_palette(n=3, name="Darjeeling1")) +
  theme_minimal() + theme(legend.title = element_blank(),legend.position="top") +
  labs(caption = "Copyright: SNU IIS Global Data Center")

##############################################
###----------  Plot D : 비핵화   ----------###
##############################################
d <- rbind(news_unigrams_by_article, news_bigrams_by_article) %>%
  filter(ngram%in%c("denuclearization", "nuclear disarmament")) %>%
  mutate(ngram = recode(ngram,
                        "denuclearization" = "Denuclearization     ",
                        "nuclear disarmament" = "Nuclear Disarmament     "))

d %>%
  select(date, tf_idf_date, ngram) %>%
  distinct() %>%
  ggplot(., aes(x=date, y=tf_idf_date, group=ngram, color=ngram, shape=ngram)) + 
  geom_point(size=1, alpha=0.6) +
  stat_smooth(geom="line", size=1, alpha=0.5, method = 'lm', formula = y ~ poly(x,2), se= FALSE) +
  scale_x_date(date_minor_breaks = "1 day") + xlab("") + ylab("relative frequency") +
  scale_color_manual(values = wes_palette(n=3, name="Darjeeling1")) +
  theme_minimal() + theme(legend.title = element_blank(),legend.position="top") +
  labs(caption = "Copyright: SNU IIS Global Data Center")

##############################################
###----------    Plot E : 경제   ----------###
##############################################
e <- rbind(news_unigrams_by_article, news_bigrams_by_article) %>%
  filter(ngram%in%c("southkorea trade","northkorea trade", "korea trade", "tariff", "tpp")) %>%
  mutate(ngram = recode(ngram, "southkorea trade" = "Korea Trade     ",
                        "northkorea trade" = "Korea Trade     ",
                        "korea trade" = "Korea Trade     ",
                        "tariff" = "Tariff     ",
                        "tpp" = "TPP     "))

e %>%
  select(date, tf_idf_date, ngram) %>%
  distinct() %>%
  ggplot(., aes(x=date, y=tf_idf_date, group=ngram, color=ngram, shape=ngram)) + 
  geom_point(size=1, alpha=0.6) +
  stat_smooth(geom="line", size=1, alpha=0.5, method = 'lm', formula = y ~ poly(x,2), se= FALSE) +
  scale_x_date(date_minor_breaks = "1 day") + xlab("") + ylab("relative frequency") +
  scale_color_manual(values = wes_palette(n=3, name="Darjeeling1")) +
  theme_minimal() + theme(legend.title = element_blank(),legend.position="top") +
  labs(caption = "Copyright: SNU IIS Global Data Center")
