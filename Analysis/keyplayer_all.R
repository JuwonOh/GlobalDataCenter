#########################
## SNU Global Data Center
## 2019 March
## Sooahn Shin
#########################
rm(list=ls())

library(tidyverse)
library(tidytext)
library(SnowballC)
library(udpipe)
library(lattice)
library(wesanderson)
library(ggraph)
library(igraph)
library(ggnet)
require(sna)

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
## Data 1
#########################
load("news_data.RData")
load("keyplayers.RData") ## 526 key players
input_data <- news_data
month.name <- c("07" ,"08" ,"09" ,"10" ,"11" ,"12", "01", "02" )
file.name <- "~/Dropbox/BigDataDiplomacy/보고서/2019/plots/keyplayer_network/news"
subtitle = "2018.7 - 2019.3"
input = "News"

## keyplayer_execution
if(Sys.getenv("LOGNAME") == "park"){
    source("~/Github/Sooahn/GlobalDataCenter/Analysis/keyplayer_execution.R")

}else{
    source("keyplayer_execution.R")
}


#########################
## Data 2
#########################
load("thinktank_data.RData")
load("keyplayers.RData") ## 526 key players
input_data <- thinktank_data
month.name <- c("07" ,"08" ,"09" ,"10" ,"11" ,"12", "01", "02", "03")
file.name <- "~/Dropbox/BigDataDiplomacy/보고서/2019/plots/keyplayer_network/thinktank"
subtitle = "2018.7 - 2019.3"
input = "Thinktank"

## keyplayer_execution
if(Sys.getenv("LOGNAME") == "park"){
    source("~/Github/Sooahn/GlobalDataCenter/Analysis/keyplayer_execution.R")

}else{
    source("keyplayer_execution.R")
}

#########################
## Data 3
#########################
load("government_data.RData")
load("keyplayers.RData") ## 526 key players
input_data <- government_data
month.name <- c("07" ,"08" ,"09" ,"10" ,"11" ,"12", "01", "02", "03")
file.name <- "~/Dropbox/BigDataDiplomacy/보고서/2019/plots/keyplayer_network/government"
subtitle = "2018.7 - 2019.3"
input = "Government"

## keyplayer_execution
if(Sys.getenv("LOGNAME") == "park"){
    source("~/Github/Sooahn/GlobalDataCenter/Analysis/keyplayer_execution.R")

}else{
    source("keyplayer_execution.R")
}
