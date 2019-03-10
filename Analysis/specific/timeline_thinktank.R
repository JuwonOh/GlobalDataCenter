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
library(gridExtra)

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

#########################
## Data 3
#########################
load("thinktank_data.RData")
input_data <- thinktank_data
month.name <- c("07" ,"08" ,"09" ,"10" ,"11" ,"12", "01", "02" )
file.name <- "~/Dropbox/BigDataDiplomacy/보고서/2019/plots/timeline/thinktank"
subtitle = "2018.7 - 2019.3"
input = "Thinktank"

## timline_execution
if(Sys.getenv("LOGNAME") == "park"){
    source("~/Github/Sooahn/GlobalDataCenter/Analysis/timline_execution.R")

}else{
    source("timline_execution.R")
}

#########################
## Data 4
#########################
load("reddit_data.RData")
input_data <- as_tibble(reddit_data)
month.name <- c("07" ,"08" ,"09" ,"10" ,"11" ,"12", "01", "02" )
file.name <- "~/Dropbox/BigDataDiplomacy/보고서/2019/plots/timeline/reddit"
subtitle = "2018.7 - 2019.3"
input = "Reddit"
## note that unnest_tokens.data.frame(, collapse = F)!!!!

## timline_execution
if(Sys.getenv("LOGNAME") == "park"){
    source("~/Github/Sooahn/GlobalDataCenter/Analysis/timline_execution.R")

}else{
    source("timline_execution.R")
}
