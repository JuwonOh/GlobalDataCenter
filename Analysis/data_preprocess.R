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
      data <- melt(data) %>% mutate(id = row_number(), source = sources[i])
      colnames(data)[2] <- "key"
      data <- data[c("id","key","value","source")]
      all_data <- rbind(all_data, data)
    }
  }
}

unique(all_data$key)
all_data$key <- recode(all_data$key, headline = "title",
                       content_url = "url",
                       time = "date",
                       blog_category = "category_type",
                       tag = "category_tag",
                       abstract = "summary",
                       timestamp = "date")
unique(all_data$key)
unique(all_data$source)

# brookings, cnas, crs, wilsoncenter include pdf
library(pdftools)
crs_ex <- pdf_text("crs_downloader/output/homesec-IF10988.pdf")
crs_ex[1]
