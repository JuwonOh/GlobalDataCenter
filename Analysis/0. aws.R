## SNU Global Data Center
## 2019 March
## Sooahn Shin
rm(list=ls())

library(aws.s3)
library(tidyverse)
load("~/Dropbox/GlobalDataCenter/Analysis/aws_info.RData")

Sys.setenv("AWS_ACCESS_KEY_ID" = get('GDCid'),
           "AWS_SECRET_ACCESS_KEY" = get('GDCkey'))
Sys.setlocale("LC_TIME", 'en_US.UTF-8')

mybucket <- get_bucket(bucket = "gdcbigdata", prefix = "data/", max = Inf)
mybucket <- data.table::rbindlist(mybucket)

files_loc <- mybucket %>% filter(Size>0) %>% select(Key) %>% unlist() %>% unname()
date.range <- seq(from = as.Date("2018/07/01"), to = as.Date("2019/03/25"), by = "day")

### Fox News
fox_loc <- files_loc[grep(paste0("data/foxnews/korea/", date.range, collapse="|"),files_loc)]

fox_df <- data.frame(id = integer(),
                     key = character(),
                     value = character())
for (i in 1:length(fox_loc)) {
  fox_data <- get_object(paste0("s3://gdcbigdata/",fox_loc[i]))
  data <- jsonlite::fromJSON(rawToChar(fox_data), flatten=TRUE)
  if (data$content!="") {
    data <- reshape::melt(data) %>% mutate(id = i)
    fox_df <- rbind(fox_df, data)
  }
  if (i%%100==0) cat(i,"th finished!\n")
}
colnames(fox_df)[2] <- "key"
save("fox_loc","fox_df", file = "~/Dropbox/GlobalDataCenter/Analysis/fox_data.RData")

### Washington Post
wp_loc <- files_loc[grep(paste0("data/wp/korea/", date.range, collapse="|"),files_loc)]

wp_df <- data.frame(id = integer(),
                     key = character(),
                     value = character())
for (i in 1:length(wp_loc)) {
  wp_data <- get_object(paste0("s3://gdcbigdata/",wp_loc[i]))
  data <- jsonlite::fromJSON(rawToChar(wp_data), flatten=TRUE)
  if (data$content!="") {
    data <- reshape::melt(data) %>% mutate(id = i)
    wp_df <- rbind(wp_df, data)
  }
  if (i%%100==0) cat(i,"th finished!\n")
}
colnames(wp_df)[2] <- "key"
save("wp_loc","wp_df", file = "~/Dropbox/GlobalDataCenter/Analysis/wp_data.RData")


### New York Times
nyt_loc <- files_loc[grep(paste0("data/nytimes/korea/", format(date.range, "%Y%m%d"), collapse="|"),files_loc)]
nyt_df <- data.frame(id = integer(),
                     key = character(),
                     value = character())
for (i in 1:length(nyt_loc)) {
  nyt_data <- get_object(paste0("s3://gdcbigdata/",nyt_loc[i]))
  data <- jsonlite::fromJSON(rawToChar(nyt_data), flatten=TRUE)
  if (data$content!="") {
    data <- reshape::melt(data) %>% mutate(id = i)
    nyt_df <- rbind(nyt_df, data)
  }
  if (i%%100==0) cat(i,"th finished!\n")
}
colnames(nyt_df)[2] <- "key"
save("nyt_loc","nyt_df", file = "~/Dropbox/GlobalDataCenter/Analysis/nyt_data.RData")

### Wall Street Journal
wsj_loc <- files_loc[grep(paste0("data/wsj/", c(format(date.range, "%B-%d-%Y"),
                                                format(date.range, "%b-%d-%Y"), "[1-2]?[0-9]-hours-ago"), collapse="|"),
                          files_loc)]
wsj_df <- data.frame(id = integer(),
                     key = character(),
                     value = character())
for (i in 1:length(wsj_loc)) {
  wsj_data <- get_object(paste0("s3://gdcbigdata/",wsj_loc[i]))
  data <- jsonlite::fromJSON(rawToChar(wsj_data), flatten=TRUE)
  data <- reshape::melt(data) %>% mutate(id = i)
  wsj_df <- rbind(wsj_df, data)
  if (i%%100==0) cat(i,"th finished!\n")
}
colnames(wsj_df)[2] <- "key"
save("wsj_loc","wsj_df", file = "~/Dropbox/GlobalDataCenter/Analysis/wsj_data.RData")

### Reddit
month.range <- seq(from = as.Date("2018/07/01"), to = as.Date("2019/03/25"), by = "month")
month.range <- str_sub(month.range,1,7)
reddit_loc <- files_loc[grep(paste0("data/reddit/RS_", month.range, collapse="|"),files_loc)]

reddit_df <- data.frame(id = integer(),
                        key = character(),
                        value = character())
for (i in 1:length(reddit_loc)) {
  data <- get_object(paste0("s3://gdcbigdata/",reddit_loc[i]))
  data <- rawToChar(data)
  data <- gsub("}\n{\"archived\"", "},{\"archived\"", data, fixed = T)
  data <- paste0("[",data,"]")
  data <- jsonlite::fromJSON(data, flatten=TRUE) %>% mutate(date = month.range[i],
                                                            id = row_number())
  data <- data %>% gather(key, value, -id, -date)
  reddit_df <- rbind(reddit_df, data)
  cat(i,"th finished!\n")
}
save("reddit_df", file = "~/Dropbox/GlobalDataCenter/Analysis/reddit_data.RData")
