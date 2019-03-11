#########################
## SNU Global Data Center
## 2019 March
## Sooahn Shin & JONG HEE PARK
#########################
rm(list=ls())

#########################
## package loading
#########################
library(tidyverse)
library(tidytext)
library(SnowballC)
library(udpipe)
library(lattice)
library(wesanderson)

#########################
## user specific working directory setup
#########################
if(Sys.getenv("LOGNAME") == "park"){
    setwd("~/Dropbox/BigDataDiplomacy/Code/2019/Analysis")
    source("~/Github/Sooahn/GlobalDataCenter/Analysis/preprocess_functions.R")

}else{
    setwd("~/Dropbox/GlobalDataCenter/Analysis")
    source("preprocess_functions.R")
}

month.name <- c("07" ,"08" ,"09" ,"10" ,"11" ,"12", "01", "02" )
subtitle = "2018.7 - 2019.3"
cut.point = 10 


#########################
## Data 1
#########################
load("news_data.RData")
input_data <- news_data
file.name <- "~/Dropbox/BigDataDiplomacy/보고서/2019/plots/frequency/news"
input = "News"

## monthly count
monthly_n <- news_data %>%
  group_by(year, month, source) %>%
  count() %>%
    mutate(date = as.Date(paste0(year, month, "01"),"%Y%m%d")) %>%
    ungroup() %>% mutate(source = recode(source, fox = "Fox",
                                         wsj = "Wall Street Journal",
                                         nyt = "New York Times",
                                         wp = "Washington Post"
                                         ))
## keyword_execution
if(Sys.getenv("LOGNAME") == "park"){
    source("~/Github/Sooahn/GlobalDataCenter/Analysis/keyword_execution.R")

}else{
    source("keyword_execution.R")
}

#########################
## Data 2
#########################
load("thinktank_data.RData")
input_data <- thinktank_data
file.name <- "~/Dropbox/BigDataDiplomacy/보고서/2019/plots/frequency/thinktank"
input = "Thinktank"

## monthly count
monthly_n <- input_data %>%
  group_by(year, month, source) %>%
  count() %>%
    mutate(date = as.Date(paste0(year, month, "01"),"%Y%m%d")) %>%
    ungroup()


## keyword_execution
if(Sys.getenv("LOGNAME") == "park"){
    source("~/Github/Sooahn/GlobalDataCenter/Analysis/keyword_execution.R")

}else{
    source("keyword_execution.R")
}


#########################
## Data 3
#########################
load("government_data.RData")
input_data <- goverment_data
file.name <- "~/Dropbox/BigDataDiplomacy/보고서/2019/plots/frequency/government"
input = "Government"

## monthly count
monthly_n <- input_data %>%
  group_by(year, month, source) %>%
  count() %>%
    mutate(date = as.Date(paste0(year, month, "01"),"%Y%m%d")) %>%
    ungroup()%>% mutate(source = recode(source, dod = "Dept. of Defense",
                                         dos = "Dept. of State",
                                        ustr = "USTR",
                                        whitehouse = "White House"
                                        ))
## keyword_execution
if(Sys.getenv("LOGNAME") == "park"){
    source("~/Github/Sooahn/GlobalDataCenter/Analysis/keyword_execution.R")

}else{
    source("keyword_execution.R")
}

#########################
## Data
#########################
load("reddit_data.RData")
input_data <- reddit_data
file.name <- "~/Dropbox/BigDataDiplomacy/보고서/2019/plots/frequency/reddit"
input = "Reddit"

## monthly count
monthly_n <- input_data %>%
  group_by(year, month) %>%
  count() %>%
    mutate(date = as.Date(paste0(year, month, "01"),"%Y%m%d")) %>%
    ungroup()

## keyword_execution
if(Sys.getenv("LOGNAME") == "park"){
    source("~/Github/Sooahn/GlobalDataCenter/Analysis/keyword_execution.R")

}else{
    source("keyword_execution.R")
}
