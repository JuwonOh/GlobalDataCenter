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

## user specific working directory setup

if(Sys.getenv("LOGNAME") == "park"){
  setwd("~/Dropbox/BigDataDiplomacy/Code/2019/Analysis")
}else{
  setwd("~/Dropbox/GlobalDataCenter/Analysis")
}

### function
## draw network plot for coocurrence of figures
coocNetworkPlot <- function(mat,  # cooccurence matrix
                            lb=10, # lower bound for coocurrence
                            layout = "nicely") {
  Fig <- as.data.frame(as.table(mat))
  Fig.freq <- Fig %>% filter(Var1==Var2)
  Fig <- Fig %>% filter(Var1!=Var2)
  colnames(Fig) <- c("figure1", "figure2","cooc")
  
  Fig <- Fig %>% filter(cooc>lb) 
  Fig.graph <- Fig %>% graph_from_data_frame()
  
  size.vertex <- Fig.freq %>% filter(Var1 %in% V(Fig.graph)$name) 
  size.vertex <- size.vertex[order(match(size.vertex$Var1, V(Fig.graph)$name)),]
  size.vertex <- size.vertex %>% select(Freq) %>% unlist()
  
  # col.vertex <- figures %>% filter(name %in% V(Fig.graph)$name)
  # col.vertex <- col.vertex[order(match(col.vertex$name, V(Fig.graph)$name)),]
  # col.vertex <- col.vertex %>% select(type) %>% unlist()
  
  if(layout=="linear"){
    plot <- Fig.graph %>%
      ggraph(layout = layout, circular="TRUE") +
      geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "grey") +
      geom_node_point(aes(size=size.vertex),alpha=0.4) +
      # geom_node_point(aes(size=size.vertex, col=col.vertex),alpha=0.4) +
      geom_node_text(aes(label = name)) +
      theme_graph(base_family = "sans") +
      theme(legend.position = "none") +
      scale_size(range = c(5, 15)) +
      # scale_color_manual(values =c("black","orange","blue","red")) +
      scale_edge_width(range = c(0.5, 5)) + scale_edge_alpha(range = c(0.4, 0.6))
  }else{
    plot <- Fig.graph %>%
      ggraph(layout = layout) +
      geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "grey") +
      geom_node_point(aes(size=size.vertex),alpha=0.4) +
      # geom_node_point(aes(size=size.vertex, col=col.vertex),alpha=0.4) +
      geom_node_text(aes(label = name)) +
      theme_graph(base_family = "sans") +
      theme(legend.position = "none") +
      scale_size(range = c(5, 15)) +
      # scale_color_manual(values =c("black","orange","blue","red")) +
      scale_edge_width(range = c(0.5, 5)) + scale_edge_alpha(range = c(0.4, 0.6))
  }
  return(plot)
}

### News Data
load("keyplayers.RData")
load("news_data.RData")

occur_matrix <- sapply(keyplayers$regex, function(x) grepl(x,news_data$text_raw))
rownames(occur_matrix) <- news_data$id_row
colnames(occur_matrix) <- keyplayers$name

### count = # of articles which mentioned the figure
keyplayers$count <- colSums(occur_matrix)
keyplayers %>%
  arrange(-count) %>%
  top_n(10,wt=count)

monthly_count <- occur_matrix %>%
  as.data.frame() %>%
  mutate(month = news_data$month) %>%
  aggregate(. ~ month, ., sum) %>%
  gather(name, count, -month) %>%
  arrange(month, -count)

monthly_top10 <- monthly_count %>%
  group_by(month) %>%
  top_n(10, wt = count)

cooc_matrix <- t(occur_matrix)%*%occur_matrix
dim(cooc_matrix)


coocNetworkPlot(cooc_matrix, lb=40)
