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

## user specific working directory setup


if(Sys.getenv("LOGNAME") == "park"){
    setwd("~/Dropbox/BigDataDiplomacy/Code/2019/Analysis")
    source("~/Github/Sooahn/GlobalDataCenter/Analysis/preprocess_functions.R")

}else{
    setwd("~/Dropbox/GlobalDataCenter/Analysis")
    source("preprocess_functions.R")
}

#########################
### News Data
load("news_data.RData")
file.name <- "~/Dropbox/BigDataDiplomacy/보고서/2019/plots/news"
subtitle = "2018.7 - 2019.3"
#########################

udmodel_english <- udpipe_load_model(file = 'english-ud-2.0-170801.udpipe')

news_data$title_prep <- prep_fun(news_data$title)
news_data$title_prep <- prep_fun2(news_data$title_prep)
news_data$title_prep[1:3]

### title - entire period
cooc_data <- coocurrence_data(news_data$title_prep)
g1 <- network_graph(cooc_data, cut.point = 120, 
                    title = "Cooccurrence Network", 
                    subtitle = subtitle,
                    edge.col = "#FF0000",
                    text.col = "#35274A",
                    default.text.size = 3,
                    default.node.size= 4)

pdf(file=paste0(file.name, "_network_total.pdf"),
    family="sans", width=8, height=8)
g1
dev.off()

### title - monthly
month.name <- c("07" ,"08" ,"09" ,"10" ,"11" ,"12", "01", "02" )

for(i in 1:length(month.name)){
    year <- ifelse(i > 6, "2019", "2018")
    cooc_data <- coocurrence_data(news_data$title_prep[news_data$month==month.name[i]])
    gg <- network_graph(cooc_data, cut.point = 60, 
                        title = paste0("Cooccurrence Network: ", year, "-", month.name[i]), 
                        default.text.size = 3,
                        default.node.size= 5)
    pdf(file=paste0(file.name, "_network_at", year,"-",month.name[i], ".pdf"),
        family="sans", width=8, height=8)
    print(gg)
    dev.off()
}
