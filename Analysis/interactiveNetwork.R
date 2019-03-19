############################################
## draw interactive plot
############################################
library(igraph)
require(intergraph)
library("networkD3")

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
## news
#########################
load("news_data.RData")
load("keyplayers.RData") ## 526 key players
input_data <- news_data
month.name <- c("07" ,"08" ,"09" ,"10" ,"11" ,"12", "01", "02", "03" )
file.name <- "~/Dropbox/BigDataDiplomacy/보고서/2019/plots/keyplayer_network/news"
subtitle = "2018.7 - 2019.3"
input = "News"

#########################
## data input
#########################
select <- dplyr::select
occur_matrix <- sapply(keyplayers$regex, function(x) grepl(x,input_data$text_raw))
rownames(occur_matrix) <- input_data$id_row
colnames(occur_matrix) <- keyplayers$name

### count = # of articles which mentioned the figure
keyplayers$count <- colSums(occur_matrix)
keyplayers %>%
  arrange(-count) %>%
  top_n(10,wt=count)

monthly_count <- occur_matrix %>%
  as.data.frame() %>%
  mutate(month = input_data$month) %>%
  aggregate(. ~ month, ., sum) %>%
  gather(name, count, -month) %>%
  arrange(month, -count)

monthly_top10 <- monthly_count %>%
  group_by(month) %>%
  top_n(10, wt = count)

cooc_matrix <- t(occur_matrix)%*%occur_matrix
dim(cooc_matrix)
rownames(cooc_matrix) <- keyplayers$name
colnames(cooc_matrix) <- keyplayers$name

## drop some nodes less than 50
drop <- rowSums(cooc_matrix) > mean(rowSums(cooc_matrix))
cooc_mat0 <- cooc_matrix[drop, drop]
cooc_mat <- ifelse(cooc_mat0 > mean(cooc_mat0)+10, 1, 0)
net = network(cooc_mat, directed = FALSE)
names = keyplayers$name[drop]
network.vertex.names(net) = names
## vertex attributes
net %v% "type" = ifelse(names %in% c("Donald Trump","Kim Jong-un","Moon Jae-in", "Shinzo Abe", "Vladimir Putin",
                                     "Xi Jinping", "Kim Dae-jung", "Barack Obama", "Lee Myong-bak", "Park Chung-hee",
                                     "Park Geun-hye" , "Bill Clinton", "George W. Bush",
                                     "H.W. Bush", "Jimmy Carter" , "Emmanuel Macron", "Angela Merkel", 
                                     "Kim Il-sung", "Ronald Reagan", "Richard M. Nixon" ,"Justin Trudeau" , "Harry Truman" , 
                                     "Kim Jong-il" ), "Leader", "Expert")
net %v% "color" = ifelse(net %v% "type" == "Leader", "steelblue", "tomato")
############################################
## interactive plotting
############################################
# Load igraph
g1 <- asIgraph(net)
# Convert to object suitable for networkD3
g_d3 <- igraph_to_networkD3(g1)
g_d3$links$source <- g_d3$links$source   
g_d3$links$target <- g_d3$links$target
g_d3$nodes$group <- as.numeric(factor(net %v% "type")) - 1
g_d3$nodes$name <- network.vertex.names(net)

# Create force directed network plot
d3 <- forceNetwork(Links = g_d3$links, Nodes = g_d3$nodes,
                   Source = 'source', Target = 'target', 
                   NodeID = 'name',  Group = 'name',
                    linkWidth = 0.5,
                   fontSize = 10,
                   radiusCalculation = "Math.sqrt(d.nodesize)+6",
                   linkDistance = 75, zoom=TRUE, ## legend=TRUE,
                   opacity = 0.7, charge=-100, ## bounded = TRUE,
                   opacityNoHover = TRUE,
                   height = 800, width = 1000)

saveNetwork(d3, "d3_news.html", selfcontained = TRUE)


#########################
## Think tank
#########################
load("thinktank_data.RData")
load("keyplayers.RData") ## 526 key players
input_data <- thinktank_data
month.name <- c("07" ,"08" ,"09" ,"10" ,"11" ,"12", "01", "02", "03")
file.name <- "~/Dropbox/BigDataDiplomacy/보고서/2019/plots/keyplayer_network/thinktank"
subtitle = "2018.7 - 2019.3"
input = "Thinktank"

#########################
## data input
#########################
select <- dplyr::select
occur_matrix <- sapply(keyplayers$regex, function(x) grepl(x,input_data$text_raw))
rownames(occur_matrix) <- input_data$id_row
colnames(occur_matrix) <- keyplayers$name

### count = # of articles which mentioned the figure
keyplayers$count <- colSums(occur_matrix)
keyplayers %>%
  arrange(-count) %>%
  top_n(10,wt=count)

monthly_count <- occur_matrix %>%
  as.data.frame() %>%
  mutate(month = input_data$month) %>%
  aggregate(. ~ month, ., sum) %>%
  gather(name, count, -month) %>%
  arrange(month, -count)

monthly_top10 <- monthly_count %>%
  group_by(month) %>%
  top_n(10, wt = count)

cooc_matrix <- t(occur_matrix)%*%occur_matrix
dim(cooc_matrix)
rownames(cooc_matrix) <- keyplayers$name
colnames(cooc_matrix) <- keyplayers$name

## drop some nodes less than 50
drop <- rowSums(cooc_matrix) > 50
cooc_mat0 <- cooc_matrix[drop, drop]
cooc_mat <- ifelse(cooc_mat0 > 5, 1, 0)
net = network(cooc_mat, directed = FALSE)
names = keyplayers$name[drop]
network.vertex.names(net) = names
## vertex attributes
net %v% "type" = ifelse(names %in% c("Donald Trump","Kim Jong-un","Moon Jae-in", "Shinzo Abe", "Vladimir Putin",
                                     "Xi Jinping", "Kim Dae-jung", "Barack Obama", "Lee Myong-bak", "Park Chung-hee",
                                     "Park Geun-hye" , "Bill Clinton", "George W. Bush",
                                     "H.W. Bush", "Jimmy Carter" , "Emmanuel Macron", "Angela Merkel", 
                                     "Kim Il-sung", "Ronald Reagan", "Richard M. Nixon" ,"Justin Trudeau" , "Harry Truman" , 
                                     "Kim Jong-il" ), "Leader", "Expert")
net %v% "color" = ifelse(net %v% "type" == "Leader", "steelblue", "tomato")
############################################
## interactive plotting
############################################
# Load igraph
g1 <- asIgraph(net)
# Convert to object suitable for networkD3
g_d3 <- igraph_to_networkD3(g1)
g_d3$links$source <- g_d3$links$source   
g_d3$links$target <- g_d3$links$target
g_d3$nodes$group <- as.numeric(factor(net %v% "type")) - 1
g_d3$nodes$name <- network.vertex.names(net)

# Create force directed network plot
d3 <- forceNetwork(Links = g_d3$links, Nodes = g_d3$nodes,
                   Source = 'source', Target = 'target', 
                   NodeID = 'name',
                   Group = 'name', linkWidth = 0.5,
                   fontSize = 20,
                   radiusCalculation = "Math.sqrt(d.nodesize)+6",
                   linkDistance = 75, zoom=TRUE, ## legend=TRUE,
                   opacity = 0.7, charge=-500, ## bounded = TRUE,
                   opacityNoHover = TRUE,
                   height = 500, width = 1000)

saveNetwork(d3, "d3_thinktank.html", selfcontained = TRUE)
#########################
## government
#########################
load("government_data.RData")
load("keyplayers.RData") ## 526 key players
input_data <- government_data
month.name <- c("07" ,"08" ,"09" ,"10" ,"11" ,"12", "01", "02", "03")
file.name <- "~/Dropbox/BigDataDiplomacy/보고서/2019/plots/keyplayer_network/government"
subtitle = "2018.7 - 2019.3"
input = "Government"

#########################
## data input
#########################
select <- dplyr::select
occur_matrix <- sapply(keyplayers$regex, function(x) grepl(x,input_data$text_raw))
rownames(occur_matrix) <- input_data$id_row
colnames(occur_matrix) <- keyplayers$name

### count = # of articles which mentioned the figure
keyplayers$count <- colSums(occur_matrix)
keyplayers %>%
  arrange(-count) %>%
  top_n(10,wt=count)

monthly_count <- occur_matrix %>%
  as.data.frame() %>%
  mutate(month = input_data$month) %>%
  aggregate(. ~ month, ., sum) %>%
  gather(name, count, -month) %>%
  arrange(month, -count)

monthly_top10 <- monthly_count %>%
  group_by(month) %>%
  top_n(10, wt = count)

cooc_matrix <- t(occur_matrix)%*%occur_matrix
dim(cooc_matrix)
rownames(cooc_matrix) <- keyplayers$name
colnames(cooc_matrix) <- keyplayers$name

## drop some nodes less than 50
drop <- rowSums(cooc_matrix) > 50
cooc_mat0 <- cooc_matrix[drop, drop]
cooc_mat <- ifelse(cooc_mat0 > 5, 1, 0)
net = network(cooc_mat, directed = FALSE)
names = keyplayers$name[drop]
network.vertex.names(net) = names
## vertex attributes
net %v% "type" = ifelse(names %in% c("Donald Trump","Kim Jong-un","Moon Jae-in", "Shinzo Abe", "Vladimir Putin",
                                     "Xi Jinping", "Kim Dae-jung", "Barack Obama", "Lee Myong-bak", "Park Chung-hee",
                                     "Park Geun-hye" , "Bill Clinton", "George W. Bush",
                                     "H.W. Bush", "Jimmy Carter" , "Emmanuel Macron", "Angela Merkel", 
                                     "Kim Il-sung", "Ronald Reagan", "Richard M. Nixon" ,"Justin Trudeau" , "Harry Truman" , 
                                     "Kim Jong-il" ), "Leader", "Expert")
net %v% "color" = ifelse(net %v% "type" == "Leader", "steelblue", "tomato")
############################################
## interactive plotting
############################################
# Load igraph
g1 <- asIgraph(net)
# Convert to object suitable for networkD3
g_d3 <- igraph_to_networkD3(g1)
g_d3$links$source <- g_d3$links$source   
g_d3$links$target <- g_d3$links$target
g_d3$nodes$group <- as.numeric(factor(net %v% "type")) - 1
g_d3$nodes$name <- network.vertex.names(net)

# Create force directed network plot
d3 <- forceNetwork(Links = g_d3$links, Nodes = g_d3$nodes,
                   Source = 'source', Target = 'target', 
                   NodeID = 'name',
                   Group = 'name', linkWidth = 0.5,
                   fontSize = 20,
                   radiusCalculation = "Math.sqrt(d.nodesize)+6",
                   linkDistance = 75, zoom=TRUE, ## legend=TRUE,
                   opacity = 0.7, charge=-500, ## bounded = TRUE,
                   opacityNoHover = TRUE,
                   height = 500, width = 1000)
saveNetwork(d3, "d3_government.html", selfcontained = TRUE)

