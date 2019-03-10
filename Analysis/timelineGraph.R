
p.list = lapply(1:length(word.list), function(i) {
        title = paste0("Bigram Words Related with ", word.list[[i]], " in " input, " Articles") ;
        df.bigram %>% filter(str_detect(ngram, word.list[i]))%>%
            ## filter(ngram%in% word.list[[i]]) %>%
            select(date, n_month, ngram) %>%
            distinct() %>%
            ggplot(., aes(x=as.Date(date), y=n_month, color=ngram), group=ngram) + 
            geom_point(size=2, alpha=0.9) +
            scale_x_date(date_minor_breaks = "1 day", date_labels = "%Y-%m") +
            xlab("Month") + ylab("Absolute Frequency") +
            theme(axis.text.x = element_text(angle = 25, vjust = 1.0, hjust = 1.0)) + 
            theme_minimal() +
            theme(legend.key=element_blank(), legend.key.size=unit(0.5,"point")) +
            guides(colour=guide_legend(ncol=12)) + 
            theme(legend.title = element_blank(),legend.text=element_text(size=rel(0.5)),
                  legend.position="bottom") +
            labs(title=title, subtitle = subtitle, caption = "Copyright: SNU IIS Global Data Center")
    })
    for(i in 1:length(word.list)){
        pdf(file=paste0(file.name, "_", word.list[[i]], "_bigram.pdf"), family="sans",
            width = 12, height = 7)
        print(p.list[[i]])
        dev.off()
        png(file=paste0(file.name, "_", word.list[[i]], "_bigram.png"), family="sans",
            width = 205, height = 205, units='mm', res = 150)
        print(p.list[[i]])
        dev.off()
    }
}
