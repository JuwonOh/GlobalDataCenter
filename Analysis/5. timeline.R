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
load("news_data.RData")
input_data <- news_data
month.name <- c("07" ,"08" ,"09" ,"10" ,"11" ,"12", "01", "02" )
file.name <- "~/Dropbox/BigDataDiplomacy/보고서/2019/plots/news_timeline"
subtitle = "2018.7 - 2019.3"
input = "News"

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


##################################################
## timeline plot: Replications of Previous Report
##################################################

input_unigrams_by_article <- input_unigrams_by_article %>%
  group_by(date,ngram) %>%
  mutate(n_date = n(), tf_idf_date = sum(tf_idf)) %>%
  ungroup()

input_bigrams_by_article <- input_bigrams_by_article %>%
  group_by(date,ngram) %>%
  mutate(n_date = n(), tf_idf_date = sum(tf_idf)) %>%
    ungroup()

##############################################
## New words for 2019
##############################################

df <- rbind(input_unigrams_by_article, input_bigrams_by_article) 
df.bigram <- input_bigrams_by_article

word.list <- c("iran", "libya", "venezuela")

##     "china", "japan", "russia", "war", 
##     "impeach", "scandal", "mueller", "russia",
##     "plutonium", "dismantlement", "uranium")
## c <- rbind(input_unigrams_by_article, input_bigrams_by_article) %>% filter(ngram%in% word.list) 

p.list = lapply(1:length(word.list), function(i) {
    df.bigram %>% filter(str_detect(ngram, word.list[i]))%>%
    ## filter(ngram%in% word.list[[i]]) %>%
    select(date, n_month, ngram) %>%
    distinct() %>%
    ggplot(., aes(x=date, y=n_month, color=ngram), group=ngram) + 
    geom_point(size=2, alpha=0.9) +
    stat_smooth(geom="line", span = 0.2, size=1, alpha=0.3) + ##, method = 'lm', formula = y ~ poly(x,3), se= FALSE) +
    scale_x_date(date_minor_breaks = "1 day", date_labels = "%Y-%m") + xlab("Month") + ylab("Absolute Frequency") +
    theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) + 
    ## scale_color_manual(values = wes_palette(n=length(word.list), name="Darjeeling1")) +
    theme_minimal() + theme(legend.title = element_blank(),legend.position="top") +
    labs(caption = "Copyright: SNU IIS Global Data Center")
})

pdf(file="~/Dropbox/BigDataDiplomacy/보고서/2019/plots/timeline_wordlist.pdf",
    family="sans")
do.call(grid.arrange, c(p.list, ncol=1))
dev.off()





###----------  Plot A : 긴장국면 ----------###
a <- rbind(input_unigrams_by_article, input_bigrams_by_article) %>%
  filter(ngram%in%c("bloody nose", "maximum pressure", "preemptive strike")) %>%
  mutate(ngram = recode(ngram, "bloody nose" = "Bloody Nose     ",
         "maximum pressure" = "Maximum Pressure     ",
         "preemptive strike" = "Preemptive Strike     "))

# relative frequency = tf_idf_date
p2 <- a %>%
    select(date, tf_idf_date, ngram) %>%
    distinct() %>%
    ggplot(., aes(x=date, y=tf_idf_date, group=ngram, color=ngram, shape=ngram)) + 
    geom_point(size=1, alpha=0.9) +
    stat_smooth(geom="line", size=1, alpha=0.3, method = 'lm', formula = y ~ poly(x,3), se= FALSE) +
    scale_x_date(date_minor_breaks = "1 day", date_labels = "%Y-%m") + xlab("") + ylab("Relative Frequency") +
    scale_color_manual(values = wes_palette(n=3, name="Darjeeling1")) +
    theme_minimal() + theme(legend.title = element_blank(),legend.position="top") +
    labs(caption = "Copyright: SNU IIS Global Data Center")

pdf(file=paste0(file.name, "_tension.pdf",
    family="sans", width=12, height=8)
p2
dev.off()

##############################################
###----------  Plot B : 대화국면 ----------###
##############################################
b <- rbind(input_unigrams_by_article, input_bigrams_by_article) %>%
  filter(ngram%in%c("korea summit", "panmunjom", "peace treaty")) %>%
  mutate(ngram = recode(ngram, "korea summit" = "Korea Summit     ",
                        "panmunjom" = "Panmunjom     ",
                        "peace treaty" = "Peace Treaty     "))

p3 <- b %>%
    select(date, tf_idf_month, ngram) %>%
    distinct() %>%
    ggplot(., aes(x=date, y=tf_idf_month, group=ngram, color=ngram, shape=ngram)) + 
    geom_point(size=1, alpha=0.9) +
    ## geom_line(size=1, alpha=0.5) +
    stat_smooth(geom="line", size=1, alpha=0.3, method = 'lm', formula = y ~ poly(x,3), se= FALSE) +
    scale_x_date(date_minor_breaks = "1 day", date_labels = "%Y-%m") + ylab("Relative Frequency") +
    scale_color_manual(values = wes_palette(n=3, name="Darjeeling1")) +
    theme_minimal() + theme(legend.title = element_blank(),legend.position="top") +
    labs(caption = "Copyright: SNU IIS Global Data Center") 

pdf(file="~/Dropbox/BigDataDiplomacy/보고서/2019/plots/timeline_dialogue.pdf",
    family="sans", width=12, height=8)
p3
dev.off()


##############################################
###----------    Plot C : 북핵   ----------###
##############################################
c <- rbind(input_unigrams_by_article, input_bigrams_by_article) %>%
  filter(ngram%in%c("northkorea iran", "nuclear test", "nuclearweapon")) %>%
  mutate(ngram = recode(ngram, "northkorea iran" = "North Korea Iran     ",
                        "nuclear test" = "Nuclear Test     ",
                        "nuclearweapon" = "Nuclear Weapon     "))

p4 <- c %>%
    select(date, tf_idf_date, ngram) %>%
    distinct() %>%
    ggplot(., aes(x=date, y=tf_idf_date, group=ngram, color=ngram, shape=ngram)) + 
    geom_point(size=1, alpha=0.9) +
    stat_smooth(geom="line", size=1, alpha=0.3, method = 'lm', formula = y ~ poly(x,3), se= FALSE) +
    scale_x_date(date_minor_breaks = "1 day", date_labels = "%Y-%m") + ylab("Relative Frequency") +
    scale_color_manual(values = wes_palette(n=3, name="Darjeeling1")) +
    theme_minimal() + theme(legend.title = element_blank(),legend.position="top") +
    labs(caption = "Copyright: SNU IIS Global Data Center")

pdf(file="~/Dropbox/BigDataDiplomacy/보고서/2019/plots/timeline_nuclear.pdf",
    family="sans", width=12, height=8)
p4
dev.off()

##############################################
###----------  Plot D : 비핵화   ----------###
##############################################
d <- rbind(input_unigrams_by_article, input_bigrams_by_article) %>%
  filter(ngram%in%c("denuclearization", "nuclear disarmament")) %>%
  mutate(ngram = recode(ngram,
                        "denuclearization" = "Denuclearization     ",
                        "nuclear disarmament" = "Nuclear Disarmament     "))

p5 <- d %>%
    select(date, tf_idf_date, ngram) %>%
    distinct() %>%
    ggplot(., aes(x=date, y=tf_idf_date, group=ngram, color=ngram, shape=ngram)) + 
    geom_point(size=1, alpha=0.9) +
    stat_smooth(geom="line", size=1, alpha=0.3, method = 'lm', formula = y ~ poly(x,3), se= FALSE) +
    scale_x_date(date_minor_breaks = "1 day", date_labels = "%Y-%m") + ylab("Relative Frequency") +
    scale_color_manual(values = wes_palette(n=3, name="Darjeeling1")) +
    theme_minimal() + theme(legend.title = element_blank(),legend.position="top") +
    labs(caption = "Copyright: SNU IIS Global Data Center")


pdf(file="~/Dropbox/BigDataDiplomacy/보고서/2019/plots/timeline_denuclear.pdf",
    family="sans", width=12, height=8)
p5
dev.off()

##############################################
###----------    Plot E : 경제   ----------###
##############################################
e <- rbind(input_unigrams_by_article, input_bigrams_by_article) %>%
  filter(ngram%in%c("southkorea trade","northkorea trade", "korea trade", "tariff", "tpp")) %>%
  mutate(ngram = recode(ngram, "southkorea trade" = "Korea Trade     ",
                        "northkorea trade" = "Korea Trade     ",
                        "korea trade" = "Korea Trade     ",
                        "tariff" = "Tariff     ",
                        "tpp" = "TPP     "))

p6 <- e %>%
    select(date, tf_idf_date, ngram) %>%
    distinct() %>%
    ggplot(., aes(x=date, y=tf_idf_date, group=ngram, color=ngram, shape=ngram)) + 
    geom_point(size=1, alpha=0.9) +
    stat_smooth(geom="line", size=1, alpha=0.3, method = 'lm', formula = y ~ poly(x,3), se= FALSE) +
    scale_x_date(date_minor_breaks = "1 day", date_labels = "%Y-%m") + ylab("Relative Frequency") +
    scale_color_manual(values = wes_palette(n=3, name="Darjeeling1")) +
    theme_minimal() + theme(legend.title = element_blank(),legend.position="top") +
    labs(caption = "Copyright: SNU IIS Global Data Center")

pdf(file="~/Dropbox/BigDataDiplomacy/보고서/2019/plots/timeline_econ.pdf",
    family="sans", width=12, height=8)
p6
dev.off()


