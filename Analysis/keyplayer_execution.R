
#########################
## plot
#########################
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


title = paste0("Keyplayer Network in ", input, " Articles") ; 
pdf(file=paste0(file.name, "_network.pdf"), family="sans", width=14, height=10)
coocNetworkPlot(cooc_matrix, lb=5, title=title, subtitle=subtitle, layout="fr")
dev.off()

#############################
## black and vertex attributes
#############################
net = network(cooc_matrix, directed = FALSE)
names = keyplayers$name
network.vertex.names(net) = names
## vertex attributes
net %v% "type" = ifelse(names %in% c("Donald Trump","Kim Jong-un","Moon Jae-in", "Shinzo Abe", "Vladimir Putin",
                                     "Xi Jinping", "Kim Dae-jung", "Barack Obama", "Lee Myong-bak", "Park Chung-hee",
                                     "Park Geun-hye" , "Bill Clinton", "George W. Bush",
                                     "H.W. Bush", "Jimmy Carter" , "Emmanuel Macron", "Angela Merkel", 
                                     "Kim Il-sung", "Ronald Reagan", "Richard M. Nixon" ,"Justin Trudeau" , "Harry Truman" , 
                                     "Kim Jong-il" ), "Leader", "Expert")
net %v% "color" = ifelse(net %v% "type" == "Leader", "steelblue", "tomato")

gg <- ggnet2(net, color="color", size = "degree", label = TRUE, label.size = 3,
                  size.min = floor(mean(rowSums(cooc_matrix)))+1, label.color = "white", edge.color = "grey") +
    theme(legend.position="none", panel.background = element_rect(fill = "grey15")) +
    labs(title=title, subtitle=subtitle, caption = "Copyright: SNU IIS Global Data Center")

pdf(file=paste0(file.name, "_network_black.pdf"), family="sans", width=14, height=10)
print(gg)
dev.off()
png(file=paste0(file.name, "_network_black.png"), family="sans",
        width = 365, height = 225, units='mm', res = 300)
print(gg)
dev.off()




#############################
## month by month graph
#############################
N.month <- length(month.name)
g <- gg <- gg.multi <- as.list(rep(NA, N.month))
time.stamp <- c(paste0("2018-", 7:12), paste0("2019-", 1:3))
library(ggnet)
require(sna)
for(t in 1:N.month){
     occur_matrix_month <- occur_matrix[input_data$month == month.name[t],]
     cooc_matrix_month <- t(occur_matrix_month)%*%occur_matrix_month
     ## dim(cooc_matrix_month)
     ## rownames(cooc_matrix_month) <- keyplayers$name
     ## colnames(cooc_matrix_month) <- keyplayers$name

     net = network(cooc_matrix_month, directed = FALSE)
     names = keyplayers$name
     network.vertex.names(net) = names
     ## vertex attributes
     net %v% "type" = ifelse(names %in% c("Donald Trump","Kim Jong-un","Moon Jae-in", "Shinzo Abe", "Vladimir Putin",
                                          "Xi Jinping", "Kim Dae-jung", "Barack Obama", "Lee Myong-bak", "Park Chung-hee",
                                          "Park Geun-hye" , "Bill Clinton", "George W. Bush",
                                          "H.W. Bush", "Jimmy Carter" , "Emmanuel Macron", "Angela Merkel", 
                                          "Kim Il-sung", "Ronald Reagan", "Richard M. Nixon" ,"Justin Trudeau" , "Harry Truman" , 
                                          "Kim Jong-il" ), "Leader", "Expert")
     net %v% "color" = ifelse(net %v% "type" == "Leader", "steelblue", "tomato")
     
     ## gg[[t]] <- ggnet2(net, color="color", size = "degree", label = TRUE, label.size = 3,
     ##                   size.min = floor(mean(rowSums(cooc_matrix_month)))+1, label.color = "white", edge.color = "grey") +
     ##     theme(legend.position="none", panel.background = element_rect(fill = "grey15")) +
     ##     labs(title=paste0("Key Figure Network at ", time.stamp[t]),
     ##          caption = "Copyright: SNU IIS Global Data Center")
     ## if(floor(mean(rowSums(cooc_matrix_month)))>=1){
     gg.multi[[t]] <- ggnet2(net, color="color", size = "degree", label = TRUE, label.size = 3,
                             size.min = floor(mean(rowSums(cooc_matrix_month)))+1, label.color = "white", edge.color = "grey") +
         theme(legend.position="none", panel.background = element_rect(fill = "grey15")) +
         labs(title=time.stamp[t])
     ## }
     cat("Loop is at t = ", t, "! \n")

}

require(NetworkChange)
pdf(file = paste0(file.name, "_network_montly1.pdf"), width=10, height=12, family="sans")
multiplot(gg.multi[[1]], gg.multi[[2]],
          gg.multi[[3]], gg.multi[[4]],
          ## gg.multi[[4+1]], gg.multi[[4+2]], gg.multi[[4+3]], gg.multi[[4+4]],
          ## gg.multi[[8+1]], gg.multi[[8+2]], ## gg.multi[[8+3]], ## gg.multi[[8+4]],
          ## gg.multi[[12+1]], gg.multi[[12+2]], gg.multi[[12+3]], gg.multi[[12+4]],
          ## gg.multi[[16+1]],
          ##gg.multi[[16+2]], gg.multi[[16+3]], gg.multi[[16+4]],
          ## gg.multi[[20+1]], gg.multi[[20+2]], gg.multi[[20+3]], gg.multi[[20+4]],
          cols = 2)
dev.off()
png(file=paste0(file.name, "_network_montly1.png"), family="sans",
        width = 365, height = 405, units='mm', res = 150)
multiplot(gg.multi[[1]], gg.multi[[2]],
          gg.multi[[3]], gg.multi[[4]],
          ## gg.multi[[4+1]], gg.multi[[4+2]], gg.multi[[4+3]], gg.multi[[4+4]],
          ## gg.multi[[8+1]], gg.multi[[8+2]], ## gg.multi[[8+3]], ## gg.multi[[8+4]],
          ## gg.multi[[12+1]], gg.multi[[12+2]], gg.multi[[12+3]], gg.multi[[12+4]],
          ## gg.multi[[16+1]],
          ##gg.multi[[16+2]], gg.multi[[16+3]], gg.multi[[16+4]],
          ## gg.multi[[20+1]], gg.multi[[20+2]], gg.multi[[20+3]], gg.multi[[20+4]],
          cols = 2)
dev.off()

pdf(file = paste0(file.name, "_network_montly2.pdf"), width=10, height=8, family="sans")
multiplot(## gg.multi[[1]], gg.multi[[2]],
          ## gg.multi[[3]], gg.multi[[4]],
          gg.multi[[4+1]], gg.multi[[4+2]], gg.multi[[4+3]], gg.multi[[4+4]],
          ## gg.multi[[8+1]], gg.multi[[8+2]], ## gg.multi[[8+3]], ## gg.multi[[8+4]],
          ## gg.multi[[12+1]], gg.multi[[12+2]], gg.multi[[12+3]], gg.multi[[12+4]],
          ## gg.multi[[16+1]],
          ##gg.multi[[16+2]], gg.multi[[16+3]], gg.multi[[16+4]],
          ## gg.multi[[20+1]], gg.multi[[20+2]], gg.multi[[20+3]], gg.multi[[20+4]],
          cols = 2)
dev.off()
png(file=paste0(file.name, "_network_montly2.png"), family="sans",
        width = 365, height = 405, units='mm', res = 150)
multiplot(## gg.multi[[1]], gg.multi[[2]],
          ## gg.multi[[3]], gg.multi[[4]],
          gg.multi[[4+1]], gg.multi[[4+2]], gg.multi[[4+3]], gg.multi[[4+4]],
          ## gg.multi[[8+1]], gg.multi[[8+2]], ## gg.multi[[8+3]], ## gg.multi[[8+4]],
          ## gg.multi[[12+1]], gg.multi[[12+2]], gg.multi[[12+3]], gg.multi[[12+4]],
          ## gg.multi[[16+1]],
          ##gg.multi[[16+2]], gg.multi[[16+3]], gg.multi[[16+4]],
          ## gg.multi[[20+1]], gg.multi[[20+2]], gg.multi[[20+3]], gg.multi[[20+4]],
          cols = 2)
dev.off()
