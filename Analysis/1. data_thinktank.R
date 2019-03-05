## SNU Global Data Center
## 2019 Feb
## Sooahn Shin
rm(list=ls())

library(tidyverse)
library(jsonlite)
setwd("~/Dropbox/BigDataDiplomacy/Data/2019/thinktank")
folders_list <- list.files(pattern="*_output")
sources <- gsub("_output","",folders_list)
folders_list[2] <- paste0(folders_list[2],"/output")
thinktank_data <- data.frame(id = integer(),
                       key = character(),
                       value = character(),
                       source = character())
for (i in 1:length(folders_list)) {
  files_loc <- folders_list[i]
  files_list <- list.files(path = files_loc, pattern="*.json")
  if (sources[i]!="stimson") {
    data <- files_list %>%
      map_df(~fromJSON(file.path(files_loc, .), flatten = TRUE)) %>%
      mutate(id = row_number()) %>%
      gather("key","value",-id) %>%
      mutate(source = sources[i])
    thinktank_data <- rbind(thinktank_data, data)
  } else {
    for (j in 1:length(files_list)) {
      data <- fromJSON(file.path(files_loc, files_list[j]), flatten=TRUE)
      data$content <- paste(data$content, collapse="\n")
      data$author <- paste(data$author, collapse=", ")
      data <- reshape::melt(data) %>% mutate(id = j, source = sources[i])
      colnames(data)[2] <- "key"
      data <- data[c("id","key","value","source")]
      thinktank_data <- rbind(thinktank_data, data)
    }
  }
}


unique(thinktank_data$key)
thinktank_data$key <- recode(thinktank_data$key, 
                       # headline = "title",
                       # content_url = "url",
                       time = "date",
                       blog_category = "category_type",
                       tag = "category_tag",
                       abstract = "summary",
                       timestamp = "date")
unique(thinktank_data$key)
unique(thinktank_data$source)

# synchronize date format
for (i in 1:nrow(thinktank_data)) {
  if (thinktank_data$key[i]=="date") {
    thinktank_data$value[i] <- format(as.Date(thinktank_data$value[i], tryFormats=c("%A, %B %d, %Y", "%B %d, %Y",
                                                                        "%b. %d, %Y", "%b %d, %Y", "%Y-%m-%d", 
                                                                        "%Y%m%d", "%m/%d/%Y", "%B %d")))
  }
}

# create year, month, day columns & make it tidy
thinktank_tidy <- thinktank_data  %>% 
  spread(key="key", value="value") %>%
  mutate(year = format(as.Date(date), "%Y"),
         month = format(as.Date(date), "%m"),
         day = format(as.Date(date), "%d")) %>%
  gather(key="key", value="value", -id, -source) %>% 
  filter(!is.na(value)) %>%
  arrange(source, id, key)

thinktank_data <- thinktank_tidy %>% 
  spread(key="key", value="value") %>%
  arrange(source, id) %>%
  mutate(id_row = row_number(), date = as.Date(date)) 

thinktank_data <- thinktank_data[str_detect(paste0(thinktank_data$content, thinktank_data$title, thinktank_data$summary), "(?i)korea"),] # "korea" related

save(thinktank_data, file="~/Dropbox/GlobalDataCenter/Analysis/thinktank_data.RData")
save(thinktank_tidy, file="~/Dropbox/GlobalDataCenter/Analysis/thinktank_tidy.RData")

monthly_n <- thinktank_data %>%
  group_by(year, month, source) %>%
  count() %>%
  mutate(date = as.Date(paste0(year, month, "01"),"%Y%m%d"))

ggplot(monthly_n) + 
  geom_bar(aes(x=date, y=n), stat="identity", alpha=0.75) +
  geom_line(aes(x=date, y=n, col=source))
