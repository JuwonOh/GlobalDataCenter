title = paste0(input, " Articles: Keyword Frequency") 

word.list.count <- str_count(word.list, "\\S+")
word.list.df <- sapply(word.list.count, function(x) ifelse(x==1,"df.unigram", "df.bigram"))
if(!is.na(word.sentiment)[1]){
    word.color <- sapply(word.sentiment, function(x) switch(x, "neutral"="black", "negative"="#F8766D", "positive"="#00BFC4"))
    plot.data <- list()
    for (i in 1:length(word.list)) {
        plot.data[[i]] <- get(word.list.df[i]) %>% filter(str_detect(ngram, paste0("^",word.list[i]))) %>%
            mutate(word = word.list[i], sent = factor(word.sentiment[i], levels = c("negative","neutral","positive")))
    }
    
    myColors <- c("red","black","navy")
    names(myColors) <- c("negative","neutral","positive")
    
    plot.data <- do.call(rbind, plot.data)
    
    plot.data <- plot.data %>%
        group_by(year, month, word, sent) %>%
        summarise(freq = sum(n)) %>%
        ungroup() %>%
        mutate(date = paste0(year, "-" ,month,"-01"))
    y.position <- max(plot.data$freq)
    
    filtered <- word.list %in% unique(plot.data$word)
    word.list <- word.list[filtered]
    word.color <- word.color[filtered]
    legend.color <- word.color[order(word.list)]
    
    ## reorder by sentiment ## word list should be ordered by sentiment (pos - neg)
    word.factor0 <- factor(plot.data$word , levels = word.list)
    
    plot.data$word.factor <- word.factor0 ## , levels(plot.data$word.factor)[rank(jitter(rank(word.sentiment)))])
    
    
    p <- plot.data %>%
        ggplot(., aes(x=as.Date(date), y=freq, group=word.factor, color=sent, alpha=word.factor)) + 
        geom_point(size=5) +
        geom_line(size=1, alpha=0.8) +
        scale_color_manual(values=myColors) +
        scale_x_date(date_breaks = "1 month",date_minor_breaks = "1 day", date_labels = "%Y-%m") + 
        xlab("Month") + ylab("Absolute Frequency") +
        theme_minimal() + theme(legend.title = element_blank(),legend.position="top") +
        theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0),
              legend.text=element_text(size=rel(1), margin = margin(r=10))) + 
        labs(title = title, caption = "Copyright: SNU IIS Global Data Center") +
        guides(shape = guide_legend(override.aes = list(color = legend.color)), nrow=2) +
        geom_vline(xintercept= as.Date("2019-02-27"), linetype="dotted", colour ="blue")+
        geom_vline(xintercept= as.Date("2018-11-06"), linetype="dotted", colour ="red")+
        annotate("text", x = as.Date("2019-03-02"), y = y.position, angle=270, size = 3, 
                 label = "NK-US Hanoi Summit", hjust = 0)+
        annotate("text", x = as.Date("2018-11-09"), y = y.position, angle=270, size = 3, 
                 label = "US Midterm Election", hjust = 0)
}else{
    ## if there is no word.sentiment
    ## word.color <- sapply(word.sentiment, function(x) switch(x, "neutral"="black", "negative"="#F8766D", "positive"="#00BFC4"))
    word.color <- brewer.pal(n = length(word.list), name = "Dark2")
    ## plot.data <- do.call(rbind, plot.data)
    plot.data <- list()
    for (i in 1:length(word.list)) {
        plot.data[[i]] <- get(word.list.df[i]) %>% filter(str_detect(ngram, paste0("^",word.list[i]))) %>% mutate(word = word.list[i])
    }
    plot.data <- do.call(rbind, plot.data)
  
    plot.data <- plot.data %>%
        group_by(year, month, word) %>%
        summarise(freq = sum(n)) %>%
        ungroup() %>%
        mutate(date = paste0(year, "-" ,month,"-01"))
    y.position <- max(plot.data$freq)
    
    filtered <- word.list %in% unique(plot.data$word)
    word.list <- word.list[filtered]
    word.color <- word.color[filtered]
    legend.color <- word.color[order(word.list)]
    
    ## reorder by sentiment
     
    p <- plot.data %>%
        ggplot(., aes(x=as.Date(date), y=freq, group=word, color=word)) + 
        geom_point(size=5) +
        geom_line(size=1, alpha=0.8) +
        scale_color_manual(values=word.color) +
        scale_x_date(date_breaks = "1 month",date_minor_breaks = "1 day", date_labels = "%Y-%m") + 
        xlab("Month") + ylab("Absolute Frequency") +
        theme_minimal() + theme(legend.title = element_blank(),legend.position="top") +
        theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0),
              legend.text=element_text(size=rel(1), margin = margin(r=10))) + 
        labs(title = title, caption = "Copyright: SNU IIS Global Data Center") +
        guides(shape = guide_legend(override.aes = list(color = legend.color)), nrow=2) +
        geom_vline(xintercept= as.Date("2019-02-27"), linetype="dotted", colour ="blue")+
        geom_vline(xintercept= as.Date("2018-11-06"), linetype="dotted", colour ="red")+
        annotate("text", x = as.Date("2019-03-02"), y = y.position, angle=270, size = 3, 
                 label = "NK-US Hanoi Summit", hjust = 0)+
        annotate("text", x = as.Date("2018-11-09"), y = y.position, angle=270, size = 3, 
                 label = "US Midterm Election", hjust = 0)



    }



## pdf(file=paste0(file.name, "_", paste0(word.list, collapse = "_"), ".pdf"), family="sans", width = 12, height = 7)
## print(p)
## dev.off()
png(file=paste0(file.name, "_", paste0(word.list, collapse = "_"), ".png"), family="sans", width = 205, height = 105, units='mm', res = 300)
print(p)
dev.off()

