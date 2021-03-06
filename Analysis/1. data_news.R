## SNU Global Data Center
## 2019 March
## Sooahn Shin
rm(list=ls())

library(tidyverse)
load("~/Dropbox/GlobalDataCenter/Analysis/fox_data.RData")
load("~/Dropbox/GlobalDataCenter/Analysis/nyt_data.RData")
load("~/Dropbox/GlobalDataCenter/Analysis/wsj_data.RData")
load("~/Dropbox/GlobalDataCenter/Analysis/wp_data.RData")

fox_df$source <- "fox"
nyt_df$source <- "nyt"
wsj_df$source <- "wsj"
wp_df$source <- "wp"

fox_df <- fox_df[fox_df$key!="headline",]
wp_df <- wp_df[wp_df$key!="date",]

wsj_df[wsj_df$key=="headline","key"] <- "title"
wp_df[wp_df$key=="headline","key"] <- "title"
wp_df[wp_df$key=="date_strf","key"] <- "date"

news_data <- rbind(fox_df,nyt_df,wsj_df,wp_df)
news_data <- news_data[c("id","key","value","source")]
news_data <- news_data[news_data$value!="",]
news_data$value <- as.character(news_data$value)

ts <- grep("hours ago",news_data[news_data$key=="timestamp","value"])
news_data[which(news_data$key=="timestamp")[ts],]
news_data[which(news_data$key=="scrap_time")[ts],]
ts_date <- news_data[which(news_data$key=="scrap_time")[ts],"value"] 
ts_date <- as.POSIXct(ts_date)
ts_time <- news_data[which(news_data$key=="timestamp")[ts],"value"] 
ts_time <- gsub(" hours ago","",ts_time)
ts_time <- as.numeric(ts_time)*60*60
news_data[which(news_data$key=="timestamp")[ts],"value"] <- as.character(ts_date - ts_time)
ts_date <- news_data[which(news_data$key=="timestamp")[-ts],"value"]
ts_date <- strsplit(ts_date, " ")
ts_date <- t(sapply(ts_date, `[`, 1:3))
ts_date <- paste(ts_date[,1],ts_date[,2],ts_date[,3])
news_data[which(news_data$key=="timestamp")[-ts],"value"] <- ts_date
news_data[which(news_data$key=="timestamp"),"key"] <- "date"

# synchronize date format
for (i in 1:nrow(news_data)) {
  if (news_data$key[i]=="date") {
    news_data$value[i] <- format(as.Date(news_data$value[i], tryFormats=c("%Y-%m-%d", "%b. %d, %Y","%B %d, %Y")))
  }
}

# create year, month, day columns & make it tidy
news_tidy <- news_data  %>% 
  spread(key="key", value="value") %>%
  mutate(year = format(as.Date(date), "%Y"),
         month = format(as.Date(date), "%m"),
         day = format(as.Date(date), "%d")) %>%
  gather(key="key", value="value", -id, -source) %>% 
  filter(!is.na(value)) %>%
  arrange(source, id, key)

news_data <- news_tidy %>% 
  spread(key="key", value="value") %>%
  arrange(source, id) %>%
  mutate(id_row = row_number(), date = as.Date(date)) 

save(news_tidy, file="~/Dropbox/GlobalDataCenter/Analysis/news_tidy.RData")

### preprocess
source("~/Dropbox/GlobalDataCenter/Analysis/preprocess_functions.R")

## NA -> ""
news_data$content[is.na(news_data$content)] <- ""
news_data$description[is.na(news_data$description)] <- ""
news_data$title[is.na(news_data$title)] <- ""
news_data$summary[is.na(news_data$summary)] <- ""

news_data <- news_data %>% mutate(text_raw = paste(content, description, title, summary)) 

news_data$text = prep_fun(news_data$text_raw)
news_data$text = prep_fun2(news_data$text)

## "" -> NA
news_data$content[news_data$content==""] <- NA
news_data$description[news_data$description==""] <- NA
news_data$title[news_data$title==""] <- NA
news_data$summary[news_data$summary==""] <- NA

save(news_data, file="~/Dropbox/GlobalDataCenter/Analysis/news_data.RData")
