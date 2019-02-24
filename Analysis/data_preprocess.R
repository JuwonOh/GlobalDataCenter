## SNU Global Data Center
## 2019 Feb
## Sooahn Shin

library(tidyverse)
library(jsonlite)
setwd("~/Dropbox/GlobalDataCenter")
folders_list <- list.files(pattern="*_scraper")
sources <- gsub("_scraper","",folders_list)
all_data <- data.frame(id = integer(),
                       key = character(),
                       value = character(),
                       source = character())
for (i in 1:length(folders_list)) {
  if (sources[i] %in% c("foxnews", "nytimes", "washingtonpost")) {
    files_loc <- paste0(folders_list[i],"/output/korea/")
  } else {
    files_loc <- paste0(folders_list[i],"/output/")
  }
  files_list <- list.files(path = files_loc, pattern="*.json")
  
  if (sources[i]!="Stimson") {
    data <- files_list %>%
      map_df(~fromJSON(file.path(files_loc, .), flatten = TRUE)) %>%
      mutate(id = row_number()) %>%
      gather("key","value",-id) %>%
      mutate(source = sources[i])
    all_data <- rbind(all_data, data)
  } else {
  # Stimson have some json files which contain *vector* as a value
  # ex: {
  #   ...
  #   "author": [
  #     "Courtney Weatherby",
  #     "Brian Eyler"
  #     ],
  #   ...
  # }
    for (j in 1:length(files_list)) {
      data <- fromJSON(paste0(files_loc, files_list[j]), flatten=TRUE)
      data$author <- paste(data$author, collapse=", ")
      data <- melt(data) %>% mutate(id = j, source = sources[i])
      colnames(data)[2] <- "key"
      data <- data[c("id","key","value","source")]
      all_data <- rbind(all_data, data)
    }
  }
}

unique(all_data$key)
all_data$key <- recode(all_data$key, 
                       # headline = "title",
                       # content_url = "url",
                       time = "date",
                       blog_category = "category_type",
                       tag = "category_tag",
                       abstract = "summary",
                       timestamp = "date")
unique(all_data$key)
unique(all_data$source)

# synchronize date format
for (i in 1:nrow(all_data)) {
  if (all_data$key[i]=="date") {
    all_data$value[i] <- format(as.Date(all_data$value[i], tryFormats=c("%A, %B %d, %Y", "%B %d, %Y",
                                                                        "%b. %d, %Y", "%b %d, %Y", "%Y-%m-%d", 
                                                                        "%Y%m%d", "%m/%d/%Y", "%B %d")))
  }
}

# create year, month, day columns & make it tidy
all_data <- all_data  %>% 
  spread(key="key", value="value") %>%
  mutate(year = format(as.Date(date), "%Y"),
         month = format(as.Date(date), "%m"),
         day = format(as.Date(date), "%d")) %>%
  gather(key="key", value="value", -id, -source) %>% 
  filter(!is.na(value)) %>%
  arrange(source, id, key)

# brookings, cnas, crs, wilsoncenter include pdf
library(pdftools)
crs_ex <- pdf_text("crs_downloader/output/homesec-IF10988.pdf")
crs_ex[1]
