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
wsj_loc <- files_loc[grep(paste0("data/wsj/", c(format(date.range, "%B-%d-%Y"), format(date.range, "%b-%d-%Y"), "[1-2]?[0-9]-hours-ago"), collapse="|"),files_loc)]
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
