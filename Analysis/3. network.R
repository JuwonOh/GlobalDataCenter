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
}else{
  setwd("~/Dropbox/GlobalDataCenter/Analysis")
}

### function
coocurrence_data <- function(input) {
  cat("udpipe annotate start!\n")
  udpipe_output <- udpipe_annotate(udmodel_english, input)
  udpipe_output <- data.frame(udpipe_output)
  cat("udpipe annotate finished!\n")
  cooc_data <- cooccurrence(x = subset(udpipe_output, upos %in% c("NOUN", "ADJ")), 
                            term = "lemma", 
                            group = c("doc_id"),
                            ngram_max = 4, n_min = 4)
  cooc_data <- cooc_data %>%
    filter(!term1 %in% na_unigrams) %>%
    filter(!term2 %in% na_unigrams)
  return(cooc_data)
}

network_graph <- function(cooc_data, 
                             cut.point = 200, 
                             title = NULL, 
                             subtitle = NULL,
                             edge.col = "#FF0000",
                             text.col = "#35274A") {
  wordnetwork <- cooc_data %>% top_n(cut.point, wt=cooc)
  wordnetwork <- igraph::graph_from_data_frame(wordnetwork)
  plot <- ggraph(wordnetwork, layout = "nicely") +
    geom_edge_link(aes(width = cooc, edge_alpha = cooc), 
                   edge_colour = edge.col)  +
    geom_node_text(aes(label = name), col = text.col, size = 4) +
    theme_graph(base_family = "sans") +
    theme(legend.position = "none") +
    scale_edge_width(range = c(1, 2)) + scale_edge_alpha(range = c(0.2, 0.5)) +
    labs(title=title, subtitle = subtitle, caption = "Copyright: SNU IIS Global Data Center")
  return(plot)
}


### News Data
source("preprocess_functions.R")
load("news_data.RData")
udmodel_english <- udpipe_load_model(file = 'english-ud-2.0-170801.udpipe')

news_data$title_prep <- prep_fun(news_data$title)
news_data$title_prep <- prep_fun2(news_data$title_prep)
news_data$title_prep[1:3]

### title - entire period
cooc_data <- coocurrence_data(news_data$title_prep)
network_graph(cooc_data)

### title - monthly
cooc_data <- coocurrence_data(news_data$title_prep[news_data$month=="07"])
network_graph(cooc_data, cut.point = 30)
