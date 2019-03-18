## SNU Global Data Center
## 2019 March
## Sooahn Shin
rm(list=ls())

library(tidyverse)
library(jsonlite)

setwd("~/Dropbox/BigDataDiplomacy/Data/2019/goverment")

folders_list <- list.files(pattern="*_output")
sources <- gsub("_output","",folders_list)
government_data <- data.frame(id = integer(),
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
    government_data <- rbind(government_data, data)
}


unique(government_data$key)
government_data$key <- recode(government_data$key, 
                             # headline = "title",
                             # content_url = "url",
                             time = "date",
                             blog_category = "category_type",
                             tag = "category_tag",
                             abstract = "summary",
                             timestamp = "date")
unique(government_data$key)

# crs: pdf
unique(government_data$source)

government_data <- government_data %>%
  arrange(key, id, source)


# synchronize date format
for (i in 1:nrow(government_data)) {
  if (government_data$key[i]=="date") {
    government_data$value[i] <- format(as.Date(government_data$value[i], tryFormats=c("%Y-%m-%d", "%Y-%m-%d 00:00:00", "%m/%d/%Y", "%B %d, %Y")))
  }
  # cat(i,"\n")
}

# create year, month, day columns & make it tidy
government_tidy <- government_data  %>% 
  spread(key="key", value="value") %>%
  mutate(year = format(as.Date(date), "%Y"),
         month = format(as.Date(date), "%m"),
         day = format(as.Date(date), "%d")) %>%
  gather(key="key", value="value", -id, -source) %>% 
  filter(!is.na(value)) %>%
  arrange(source, id, key)

government_data <- government_tidy %>% 
  spread(key="key", value="value") %>%
  arrange(source, id) %>%
  mutate(id_row = row_number(), date = as.Date(date)) 

# "korea" related
government_data <- government_data[str_detect(paste0(government_data$content, government_data$title), "(?i)korea"),]

save(government_tidy, file="~/Dropbox/GlobalDataCenter/Analysis/government_tidy.RData")

### preprocess
source("~/Dropbox/GlobalDataCenter/Analysis/preprocess_functions.R")

## NA -> ""
government_data$content[is.na(government_data$content)] <- ""
government_data$title[is.na(government_data$title)] <- ""

government_data <- government_data %>% mutate(text_raw = paste(content, title)) 

government_data$text = prep_fun(government_data$text_raw)
government_data$text = prep_fun2(government_data$text)

## "" -> NA
government_data$content[government_data$content==""] <- NA
government_data$title[government_data$title==""] <- NA

save(government_data, file="~/Dropbox/GlobalDataCenter/Analysis/government_data.RData")
