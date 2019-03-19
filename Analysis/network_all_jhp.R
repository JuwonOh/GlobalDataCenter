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
library(ggraph) 

#########################
## user specific working directory setup
#########################
if(Sys.getenv("LOGNAME") == "park"){
    setwd("~/Dropbox/BigDataDiplomacy/Code/2019/Analysis")
    source("~/Github/Sooahn/GlobalDataCenter/Analysis/preprocess_functions_jhp.R")

}else{
    setwd("~/Dropbox/GlobalDataCenter/Analysis")
    source("preprocess_functions_jhp.R")
}

#########################
## Data 1
#########################
load("news_data.RData")
file.name <- "~/Dropbox/BigDataDiplomacy/보고서/2019/plots/keyword_network/news"
subtitle = "2018.7 - 2019.3"
input_data <- news_data
input = "News"
cut.point = 50

## network_execution
if(Sys.getenv("LOGNAME") == "park"){
    source("~/Github/Sooahn/GlobalDataCenter/Analysis/network_execution_jhp.R")

}else{
    source("network_execution_jhp.R")
}

#########################
## Data 2
#########################
load("thinktank_data.RData")
input_data <- thinktank_data
file.name <- "~/Dropbox/BigDataDiplomacy/보고서/2019/plots/keyword_network/thinktank"
input = "Thinktank"

## network_execution
if(Sys.getenv("LOGNAME") == "park"){
    source("~/Github/Sooahn/GlobalDataCenter/Analysis/network_execution_jhp.R")

}else{
    source("network_execution_jhp.R")
}

#########################
## Data 3
#########################
load("government_data.RData")
input_data <- goverment_data
file.name <- "~/Dropbox/BigDataDiplomacy/보고서/2019/plots/keyword_network/government"
input = "Government"


## network_execution
if(Sys.getenv("LOGNAME") == "park"){
    source("~/Github/Sooahn/GlobalDataCenter/Analysis/network_execution_jhp.R")

}else{
    source("network_execution_jhp.R")
}

#########################
## Data 4
#########################
load("reddit_data.RData")
file.name <- "~/Dropbox/BigDataDiplomacy/보고서/2019/plots/keyword_network/reddit"
subtitle = "2018.7 - 2019.3"
input_data <- reddit_data
input = "Reddit"

## network_execution
if(Sys.getenv("LOGNAME") == "park"){
    source("~/Github/Sooahn/GlobalDataCenter/Analysis/network_execution_jhp.R")

}else{
    source("network_execution_jhp.R")
}
