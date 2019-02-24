## SNU Global Data Center
## 2019 Feb
## Sooahn Shin

library(tidyverse)
library(tidytext)
library(SnowballC)
library(wesanderson)
setwd("~/Dropbox/GlobalDataCenter/Analysis")

load("all_data.RData")

newsmedia <- c("foxnews", "nytimes", "washingtonpost", "wsj")
government <- c("whitehouse", "DoD", "DoS", "USTR")
thinktank <- c("bipartisan", "brooking", "Cato", "cnas", "CSIS", "freedomhouse", "humanright", "N38", "rand", "Stimson", "wilsoncenter")

news_data <- all_data %>% filter(source %in% newsmedia)
gov_data <- all_data %>% filter(source %in% government)
think_data <- all_data %>% filter(source %in% thinktank)

DAT <- news_data %>% # input: news_data / gov_data / think_data
  spread(key="key", value="value") %>%
  arrange(source, id) %>%
  mutate(id_row = row_number(), date = as.Date(date))
##############################################
###---------- 1. Unigram Keyword ----------###
##############################################
unigrams <- DAT %>%
  unnest_tokens(ngram, content, token = "ngrams", n = 1) %>%
  filter(!ngram %in% stop_words$word) %>%
  mutate(stemmed = wordStem(ngram))
###-- DOC = date
unigrams_date_counts <- unigrams %>%
  count(date, ngram) %>%
  bind_tf_idf(ngram, date, n) %>%
  arrange(desc(tf_idf))
###-- DOC = article
unigrams_article_counts <- unigrams %>%
  count(id_row, ngram) %>%
  bind_tf_idf(ngram, id_row, n) %>%
  arrange(desc(tf_idf)) %>%
  left_join(DAT[,c("id_row","date")])
unigrams_article_date <- unigrams_article_counts %>%
  group_by(date,ngram) %>%
  mutate(n_date = n(), tf_idf_date = sum(tf_idf)) %>%
  ungroup()
###-- main function
unigrams_date_counts %>%
  filter(ngram=="cvid")
unigrams_article_counts %>%
  filter(ngram=="cvid")
unigrams_article_date %>%
  filter(ngram=="cvid")
##############################################
###---------- 2. Bigram  Keyword ----------###
##############################################
bigrams <- DAT %>%
  unnest_tokens(ngram, content, token = "ngrams", n = 2)
bigrams_separated <- bigrams %>%
  separate(ngram, c("word1", "word2"), sep = " ")
bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)
bigrams_united <- bigrams_filtered %>%
  unite(ngram, word1, word2, sep = " ")
# stemmed version
bigrams_stemmed <- bigrams_filtered %>% 
  mutate(stem1 = wordStem(word1), stem2 = wordStem(word2))
bigrams_stemmed_united <- bigrams_stemmed %>%
  unite(ngram, stem1, stem2, sep = " ") %>%
  select(-word1,-word2)
###-- DOC = date
bigrams_date_counts <- bigrams_united %>%
  count(date, ngram) %>%
  bind_tf_idf(ngram, date, n) %>%
  arrange(desc(tf_idf))
###-- DOC = article
bigrams_article_counts <- bigrams_united %>%
  count(id_row, ngram) %>%
  bind_tf_idf(ngram, id_row, n) %>%
  arrange(desc(tf_idf)) %>%
  left_join(DAT[,c("id_row","date")])
bigrams_article_date <- bigrams_article_counts %>%
  group_by(date,ngram) %>%
  mutate(n_date = n(), tf_idf_date = sum(tf_idf)) %>%
  ungroup()
###-- main function
bigrams_date_counts %>%
  filter(ngram=="fair trade")
bigrams_article_counts %>%
  filter(ngram=="fair trade")
bigrams_article_date %>%
  filter(ngram=="fair trade")
##############################################
###---------- 3. Trigram Keyword ----------###
##############################################
trigrams <- DAT %>%
  unnest_tokens(ngram, content, token = "ngrams", n = 3)
trigrams_separated <- trigrams %>%
  separate(ngram, c("word1", "word2", "word3"), sep = " ")
trigrams_filtered <- trigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word[stop_words$word != "in"])
trigrams_united <- trigrams_filtered %>%
  unite(ngram, word1, word2, word3, sep = " ")
# stemmed version
trigrams_stemmed <- trigrams_filtered %>% 
  mutate(stem1 = wordStem(word1), stem2 = wordStem(word2), stem3 = wordStem(word3))
trigrams_stemmed_united <- trigrams_stemmed %>%
  unite(ngram, stem1, stem2, stem3, sep = " ") %>%
  select(-word1,-word2, -word3)
###-- DOC = date
trigrams_date_counts <- trigrams_united %>%
  count(date, ngram) %>%
  bind_tf_idf(ngram, date, n) %>%
  arrange(desc(tf_idf))
###-- DOC = article
trigrams_article_counts <- trigrams_united %>%
  count(id_row, ngram) %>%
  bind_tf_idf(ngram, id_row, n) %>%
  arrange(desc(tf_idf)) %>%
  left_join(DAT[,c("id_row","date")])
trigrams_article_date <- trigrams_article_counts %>%
  group_by(date,ngram) %>%
  mutate(n_date = n(), tf_idf_date = sum(tf_idf)) %>%
  ungroup()
###-- main function
trigrams_date_counts %>%
  filter(ngram=="moon jae in")
trigrams_article_counts %>%
  filter(ngram=="moon jae in")
trigrams_article_date %>%
  filter(ngram=="moon jae in")


##############################################
###----------  Keyword Function  ----------###
##############################################
KeywordPlot <- function(Keyword, date.to.doc = TRUE, tf.idf = TRUE) {
  keyword <- tolower(Keyword)
  num <- str_count(keyword, "\\S+")
  num <- switch(num, "1"="unigrams", "2"="bigrams", "3"="trigrams")
  doc <- ifelse(date.to.doc, "_date_counts", "_article_date")
  type <- ifelse(tf.idf, "tf_idf", "n")
  type <- ifelse(date.to.doc, type, paste0(type,"_date"))
  # plot
  Subtitle <- ifelse(date.to.doc, "(date to doc)", "(article to doc)")
  Ylab <- ifelse(tf.idf, "tf-idf", "n")
   <- get(paste0(num,doc)) %>%
    filter(ngram==keyword) %>%
    filter(get(type) == max(get(type))) %>%
    .[,type] %>%
    as.numeric()
  get(paste0(num,doc)) %>%
    filter(ngram==keyword) %>%
    ggplot(., aes(date, get(type))) + geom_line(size=1, alpha=0.7) +
    scale_x_date(date_minor_breaks = "1 day") + xlab("") + ylab(Ylab) +
    theme_classic() + ggtitle(paste("Keyword: ", Keyword), Subtitle) 
}

KeywordPlot("Moon Jae In")
KeywordPlot("Sanctions")
KeywordPlot("Human Rights")

##############################################
###----------  Plot A : 긴장국면 ----------###
##############################################
a1 <- bigrams_date_counts %>%
  filter(ngram=="bloody nose")
a2 <- bigrams_date_counts %>%
  filter(ngram=="maximum pressure")
a3 <- bigrams_date_counts %>%
  filter(ngram=="preemptive strike")
a <- rbind(a1,a2) %>% rbind(a3)
a$ngram <- a %>% select(ngram) %>% unlist %>% recode("bloody nose" = "Bloody Nose     ",
                                                     "maximum pressure" = "Maximum Pressure     ",
                                                     "preemptive strike" = "Preemptive Strike     ")
plotA <- a %>%
  ggplot(., aes(x=date, y=tf, group=ngram, color=ngram, shape=ngram)) + 
  geom_point(size=1, alpha=0.6) +
  stat_smooth(geom="line", size=1, alpha=0.5, method = 'lm', formula = y ~ poly(x,7), se= FALSE) +
  scale_x_date(date_minor_breaks = "1 day") + xlab("") + ylab("relative frequency") +
  scale_color_manual(values = wes_palette(n=3, name="Darjeeling1")) +
  theme_minimal() + theme(legend.title = element_blank(),legend.position="top") 
plotA

# pdf(file="~/Dropbox/TrumpKim/Report/plots3/1. A 긴장국면_n.pdf", family="sans", width=7, height=5)
# plotA
# dev.off()

##############################################
###----------  Plot B : 대화국면 ----------###
##############################################
b1 <- bigrams_date_counts %>%
  filter(ngram=="korea summit")
b2 <- unigrams_date_counts %>%
  filter(ngram=="panmunjom")
b3 <- bigrams_date_counts %>%
  filter(ngram=="peace treaty")
b <- rbind(b1,b2) %>% rbind(b3)
b$ngram <- b %>% select(ngram) %>% unlist %>% recode("korea summit" = "Korea Summit     ",
                                                     "panmunjom" = "Panmunjom     ",
                                                     "peace treaty" = "Peace Treaty     ")
plotB <- b %>%
  ggplot(., aes(x=date, y=tf, group=ngram, color=ngram, shape=ngram)) + 
  geom_point(size=1, alpha=0.6) +
  stat_smooth(geom="line", size=1, alpha=0.5, method = 'lm', formula = y ~ poly(x,5), se= FALSE) +
  scale_x_date(date_minor_breaks = "1 day") + xlab("") + ylab("relative frequency") +
  scale_color_manual(values = wes_palette(n=3, name="Darjeeling1")) +
  theme_minimal() + theme(legend.title = element_blank(),legend.position="top") 
plotB

# pdf(file="~/Dropbox/TrumpKim/Report/plots3/1. B 대화국면.pdf", family="sans", width=7, height=5)
# plotB
# dev.off()

##############################################
###----------    Plot C : 북핵   ----------###
##############################################
c1 <- trigrams_date_counts %>%
  filter(ngram=="north korea iran")
c2 <- bigrams_date_counts %>%
  filter(ngram=="nuclear test")
c3 <- bigrams_date_counts %>%
  filter(ngram=="nuclear weapon")
c <- rbind(c1,c2) %>% rbind(c3) 
c$ngram <- c %>% select(ngram) %>% unlist %>% recode("north korea iran" = "North Korea Iran     ",
                                                     "nuclear test" = "Nuclear Test     ",
                                                     "nuclear weapon" = "Nuclear Weapon     ")
plotC <- c %>%
  ggplot(., aes(x=date, y=tf, group=ngram, color=ngram, shape=ngram)) + 
  geom_point(size=1, alpha=0.6) +
  stat_smooth(geom="line", size=1, alpha=0.5, method = 'lm', formula = y ~ poly(x,6), se= FALSE) +
  scale_x_date(date_minor_breaks = "1 day") + xlab("") + ylab("relative frequency") +
  scale_color_manual(values = wes_palette(n=3, name="Darjeeling1")) +
  theme_minimal() + theme(legend.title = element_blank(),legend.position="top") 
plotC

# pdf(file="~/Dropbox/TrumpKim/Report/plots3/1. C 북핵.pdf", family="sans", width=7, height=5)
# plotC
# dev.off()

##############################################
###----------  Plot D : 비핵화   ----------###
##############################################
d1 <- unigrams_date_counts %>%
  filter(ngram=="cvid")
d2 <- unigrams_date_counts %>%
  filter(ngram=="denuclearization")
d3 <- bigrams_date_counts %>%
  filter(ngram=="nuclear disarmament")
d <- rbind(d1,d2) %>% rbind(d3)
d$ngram <- d %>% select(ngram) %>% unlist %>% recode("cvid" = "CVID     ",
                                                     "denuclearization" = "Denuclearization     ",
                                                     "nuclear disarmament" = "Nuclear Disarmament     ")
plotD <- d %>%
  ggplot(., aes(x=date, y=tf, group=ngram, color=ngram, shape=ngram)) + 
  geom_point(size=1, alpha=0.6) +
  stat_smooth(geom="line", size=1, alpha=0.5, method = 'lm', formula = y ~ poly(x,3), se= FALSE) +
  scale_x_date(date_minor_breaks = "1 day") + xlab("") + ylab("relative frequency") +
  scale_color_manual(values = wes_palette(n=3, name="Darjeeling1")) +
  theme_minimal() + theme(legend.title = element_blank(),legend.position="top") 
plotD

# pdf(file="~/Dropbox/TrumpKim/Report/plots3/1. D 비핵화.pdf", family="sans", width=7, height=5)
# plotD
# dev.off()

##############################################
###----------    Plot E : 경제   ----------###
##############################################
e1 <- bigrams_date_counts %>%
  filter(ngram=="korea trade")
e2 <- unigrams_date_counts %>%
  filter(ngram=="tariff")
e3 <- unigrams_date_counts %>%
  filter(ngram=="tpp")
e <- rbind(e1,e2) %>% rbind(e3) 
e$ngram <- e %>% select(ngram) %>% unlist %>% recode("korea trade" = "Korea Trade     ",
                                                     "tariff" = "Tariff     ",
                                                     "tpp" = "TPP     ")
plotE <- e %>%
  ggplot(., aes(x=date, y=tf, group=ngram, color=ngram, shape=ngram)) + 
  geom_point(size=1, alpha=0.6) +
  stat_smooth(geom="line", size=1, alpha=0.5, method = 'lm', formula = y ~ poly(x,5), se= FALSE) +
  scale_x_date(date_minor_breaks = "1 day") + xlab("") + ylab("relative frequency") +
  scale_color_manual(values = wes_palette(n=3, name="Darjeeling1")) +
  theme_minimal() + theme(legend.title = element_blank(),legend.position="top")
plotE

# pdf(file="~/Dropbox/TrumpKim/Report/plots3/1. E 경제.pdf", family="sans", width=7, height=5)
# plotE
# dev.off()