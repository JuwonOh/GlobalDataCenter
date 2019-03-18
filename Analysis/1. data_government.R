## SNU Global Data Center
## 2019 March
## Sooahn Shin
rm(list=ls())

library(tidyverse)
library(jsonlite)

setwd("~/Dropbox/BigDataDiplomacy/Data/2019/goverment")

folders_list <- list.files(pattern="*_output")
sources <- gsub("_output","",folders_list)
goverment_data <- data.frame(id = integer(),
                             key = character(),
                             value = character(),
                             source = character())
for (i in 1:length(folders_list)) {
  files_loc <- folders_list[i]
  files_list <- list.files(path = files_loc, pattern="*.json")
  data <- files_list %>%
      map_df(~fromJSON(file.path(files_loc, .), flatten = TRUE)) %>%
      mutate(id = row_number()) %>%
      gather("key","value",-id) %>%
      mutate(source = sources[i])
    goverment_data <- rbind(goverment_data, data)
}


unique(goverment_data$key)
goverment_data$key <- recode(goverment_data$key, 
                             # headline = "title",
                             # content_url = "url",
                             time = "date",
                             blog_category = "category_type",
                             tag = "category_tag",
                             abstract = "summary",
                             timestamp = "date")
unique(goverment_data$key)

# crs: pdf
unique(goverment_data$source)

goverment_data <- goverment_data %>%
  arrange(key, id, source)


# synchronize date format
for (i in 1:nrow(goverment_data)) {
  if (goverment_data$key[i]=="date") {
    goverment_data$value[i] <- format(as.Date(goverment_data$value[i], tryFormats=c("%Y-%m-%d", "%Y-%m-%d 00:00:00", "%m/%d/%Y", "%B %d, %Y")))
  }
  # cat(i,"\n")
}

# create year, month, day columns & make it tidy
goverment_tidy <- goverment_data  %>% 
  spread(key="key", value="value") %>%
  mutate(year = format(as.Date(date), "%Y"),
         month = format(as.Date(date), "%m"),
         day = format(as.Date(date), "%d")) %>%
  gather(key="key", value="value", -id, -source) %>% 
  filter(!is.na(value)) %>%
  arrange(source, id, key)

goverment_data <- goverment_tidy %>% 
  spread(key="key", value="value") %>%
  arrange(source, id) %>%
  mutate(id_row = row_number(), date = as.Date(date)) 

# "korea" related
goverment_data <- goverment_data[str_detect(paste0(goverment_data$content, goverment_data$title), "(?i)korea"),]

save(goverment_tidy, file="~/Dropbox/GlobalDataCenter/Analysis/goverment_tidy.RData")

### preprocess
source("~/Dropbox/GlobalDataCenter/Analysis/preprocess_functions.R")

## NA -> ""
goverment_data$content[is.na(goverment_data$content)] <- ""
goverment_data$title[is.na(goverment_data$title)] <- ""

goverment_data <- goverment_data %>% mutate(text_raw = paste(content, title)) 

goverment_data$text = prep_fun(goverment_data$text_raw)
goverment_data$text = prep_fun2(goverment_data$text)

## "" -> NA
goverment_data$content[goverment_data$content==""] <- NA
goverment_data$title[goverment_data$title==""] <- NA

save(goverment_data, file="~/Dropbox/GlobalDataCenter/Analysis/government_data.RData")
