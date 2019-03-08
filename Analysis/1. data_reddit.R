## SNU Global Data Center
## 2019 March
## Sooahn Shin
rm(list=ls())

library(tidyverse)
load("~/Dropbox/GlobalDataCenter/Analysis/reddit_data.RData")

# create year, month, day columns & make it tidy
reddit_data <- reddit_df  %>% 
  filter(key %in% c("author","score","title","selftext","permalink","url")) %>%
  spread(key="key", value="value") %>%
  mutate(year = sapply(strsplit(date,"-"), `[[`, 1),
         month = sapply(strsplit(date,"-"), `[[`, 2),
         id_row = row_number()) 
colnames(reddit_data)[colnames(reddit_data)=="selftext"] <- "content"
reddit_data$content[!reddit_data$content %in% c("","[deleted]")]
reddit_data <- reddit_data %>% select(-content)

### preprocess
source("~/Dropbox/GlobalDataCenter/Analysis/preprocess_functions.R")
reddit_data$text = prep_fun(reddit_data$title)
reddit_data$text = prep_fun2(reddit_data$text)

reddit_tidy <- reddit_data %>%
  gather(key="key", value="value", -id, -date) %>% 
  filter(!is.na(value)) %>%
  arrange(date, id, key)

save("reddit_data", file = "~/Dropbox/GlobalDataCenter/Analysis/reddit_data.RData")
save("reddit_tidy", file = "~/Dropbox/GlobalDataCenter/Analysis/reddit_tidy.RData")
