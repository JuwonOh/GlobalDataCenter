##################################################
## functions to execute plotting codes
## Run this after keyword_*****.R
##################################################
library(ggplot2)
library(ggforce)
library(gridExtra)
library(kableExtra)

title = paste0("Korea-related ", input,  " Article Frequency") ;
if(input =="Reddit"){
    p0 <- ggplot(monthly_n) + 
        geom_bar(aes(x=date, y=n), stat="identity", alpha=0.9) +
        geom_line(aes(x=date, y=n), size=1) +
        geom_point(aes(x=date, y=n), alpha=0.6, size = 4) +
        scale_x_date(date_breaks = "months" , date_labels = "%Y-%b") + 
        labs(title=title, subtitle = subtitle, y = "Absolute Frequency", x="Month", 
             caption = "Copyright: SNU IIS Global Data Center")
  
}else{
    p0 <- ggplot(monthly_n) + 
        geom_bar(aes(x=date, y=n), stat="identity", alpha=0.9) +
        geom_line(aes(x=date, y=n, alpha=source, col=source), size=2) +
        geom_point(aes(x=date, y=n, size=source, col=source), alpha=0.6) +
        scale_shape_manual(values = c(1:length(unique(monthly_n$source)))) +
        scale_x_date(date_breaks = "months" , date_labels = "%Y-%b") + 
        labs(title=title, subtitle = subtitle, y = "Absolute Frequency", x="Month", 
             caption = "Copyright: SNU IIS Global Data Center")

    }
    pdf(file=paste0(file.name, "_totalfreq.pdf"),
        family="sans", width=12, height=12)
    p0
    dev.off()
    png(file=paste0(file.name, "_totalfreq.png"),
        width = 405, height = 365, units='mm', res = 150)
    print(p0)
    dev.off()

########################
## unigram keyword
#########################
if(input =="Reddit"){
    input_data <- as_tibble(input_data)
    input_unigrams <- input_data %>%
        unnest_tokens(ngram, text, token = "ngrams", n = 1, collapse = F) %>%
        filter(!ngram %in% stop_words$word) %>%
        mutate(stemmed = wordStem(ngram))
    
    ## bigram keyword    
    input_bigrams <- input_data %>%
        unnest_tokens(ngram, text, token = "ngrams", n = 2, collapse = F) %>%
        separate(ngram, c("word1", "word2"), sep = " ") %>%
        filter(!word1 %in% stop_words$word) %>%
        filter(!word2 %in% stop_words$word) %>%
        unite(ngram, word1, word2, sep = " ")   
}else{
    input_unigrams <- input_data %>%
        unnest_tokens(ngram, text, token = "ngrams", n = 1) %>%
        filter(!ngram %in% stop_words$word) %>%
        mutate(stemmed = wordStem(ngram))
    
    ## bigram keyword
    input_bigrams <- input_data %>%
        unnest_tokens(ngram, text, token = "ngrams", n = 2) %>%
        separate(ngram, c("word1", "word2"), sep = " ") %>%
        filter(!word1 %in% stop_words$word) %>%
        filter(!word2 %in% stop_words$word) %>%
        unite(ngram, word1, word2, sep = " ")
    
}

input_unigrams_by_article <- input_unigrams %>%
    count(id_row, ngram) %>%
    bind_tf_idf(ngram, id_row, n) %>%
    arrange(desc(tf_idf)) %>%
    left_join(input_data[,c("id_row","year","month", "date")]) %>%
    group_by(year, month, ngram) %>%
    mutate(n_month = n(), tf_idf_month = sum(tf_idf)) %>%
    ungroup()


input_bigrams_by_article <- input_bigrams %>%
    count(id_row, ngram) %>%
    bind_tf_idf(ngram, id_row, n) %>%
    arrange(desc(tf_idf)) %>%
    left_join(input_data[,c("id_row","year","month", "date")]) %>%
    group_by(year, month, ngram) %>%
    mutate(n_month = n(), tf_idf_month = sum(tf_idf)) %>%
    ungroup()


#########################
## uniigram plot
#########################
## absolute frequency
topcut.point <- input_unigrams_by_article %>%
    dplyr::select(ngram,year,month,n_month,tf_idf_month) %>%
    distinct() %>%
    filter(!ngram %in% na_bigrams) %>%
    group_by(year,month) %>%
    top_n(cut.point, wt=jitter(n_month)) %>%
    arrange(year,month,-n_month) %>%
    mutate(time = paste(year, month, sep="-"))

pd <- topcut.point %>%
    group_by(month) %>%
    ungroup() %>%
    arrange(month, n_month) %>%
    mutate(order = row_number())

title = paste0("Unigram Absolute Frequency in Korea-related ", input, " Articles")
## make a table
time.stamp <- sort(unique(pd$time))
sumstat.list <-
    lapply(1:length(time.stamp), function(t){
        pd[pd$time == time.stamp[t], ] %>% 
            group_by(ngram) %>%
            summarize(n = sum(n_month))%>%
            arrange(desc(n))
    })
sumstat.list <- lapply(1:length(time.stamp), function(t){sumstat.list[[t]][1: cut.point, ]})
dt <- Reduce(cbind, sumstat.list)
colnames(dt) <- unlist(lapply(1:length(time.stamp), function(t){c(paste0(time.stamp[t], " : Word"),"Freq")}))
kable(dt, caption = title) %>% kable_styling(font_size = 12, full_width=FALSE) %>%
    save_kable(file = paste0(file.name, "_unigram_absolute.html"), self_contained = T)
## write.table(sumstat, file = "sumstats.txt", sep = ",", quote = FALSE, row.names = F)

## draw a picture 
p01 <- ggplot(pd, aes(order, n_month, fill = factor(month))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ time, ncol = 3, scales = "free") +
    labs(title=title, subtitle = subtitle, y = "Absolute Frequency", x="Month", 
         caption = "Copyright: SNU IIS Global Data Center") +
    scale_x_continuous(
        breaks = pd$order,
        labels = pd$ngram,
        expand = c(0,0)
    ) +
    coord_flip()

pdf(file=paste0(file.name, "_topcut.pointunigram_absolute.pdf"),
    family="sans", width=16, height=15)
print(p01)
dev.off()

png(file=paste0(file.name, "_topcut.pointunigram_absolute.png"),
    width = 405, height = 365, units='mm', res = 150)
print(p01)
dev.off()

## relative frequency
topcut.point <- input_unigrams_by_article %>%
    dplyr::select(ngram,year,month,n_month,tf_idf_month) %>%
    distinct() %>%
    filter(!ngram %in% na_bigrams) %>%
    group_by(year,month) %>%
    top_n(cut.point, wt=jitter(tf_idf_month)) %>%
    arrange(year,month,-tf_idf_month) %>%
    mutate(time = paste(year, month, sep="-"))

pd <- topcut.point %>%
    group_by(month) %>%
    ungroup() %>%
    arrange(month, tf_idf_month) %>%
    ## 3. Add order column of row numbers
    mutate(order = row_number())

title = paste0("Unigram Relative Frequency in Korea-related ", input, " Articles")
## make a table
time.stamp <- unique(pd$time)
sumstat.list <-
    lapply(1:length(time.stamp), function(t){
        pd[pd$time == time.stamp[t], ] %>% 
            group_by(ngram) %>%
            summarize(n = sum(tf_idf_month))%>%
            arrange(desc(n))
    })
sumstat.list <- lapply(1:length(time.stamp), function(t){sumstat.list[[t]][1: cut.point, ]})
dt <- Reduce(cbind, sumstat.list)
colnames(dt) <- unlist(lapply(1:length(time.stamp), function(t){c(paste0(time.stamp[t], " : Word"),"Freq")}))
kable(dt, caption = title, digits=0) %>% kable_styling(font_size = 12, full_width=FALSE) %>%
    save_kable(file = paste0(file.name, "_unigram_relative.html"), self_contained = T)

## plot
p02 <- ggplot(pd, aes(order, tf_idf_month, fill = factor(month))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ time, ncol = 3, scales = "free") + theme_grey(base_size = 22) + 
    labs(title=title, subtitle = subtitle, y = "Relative Frequency", x="Month", 
         caption = "Copyright: SNU IIS Global Data Center") +
    scale_x_continuous(
        breaks = pd$order,
        labels = pd$ngram,
        expand = c(0,0)
    ) +
    coord_flip()
pdf(file=paste0(file.name, "_topcut.pointunigram_relative.pdf"),
    family="sans", width=16, height=15)
print(p02)
dev.off()
png(file=paste0(file.name, "_topcut.pointunigram_relative.png"),
    width = 405, height = 365, units='mm', res = 150)
print(p02)
dev.off()

#########################
## bigram plot
#########################
topcut.point <- input_bigrams_by_article %>%
    dplyr::select(ngram,year,month,n_month,tf_idf_month) %>%
    distinct() %>%
    filter(!ngram %in% na_bigrams) %>%
    group_by(year,month) %>%
    top_n(cut.point, wt=jitter(n_month)) %>%
    arrange(year,month,-n_month) %>%
    mutate(time = paste(year, month, sep="-"))

## because of facetting, ordering is not working.
## So, we have to ungroup it and name a new variable "order"
pd <- topcut.point %>%
    group_by(month) %>%
    ungroup() %>%
    arrange(month, n_month) %>%
    mutate(order = row_number())

title = paste0("Bigram Absolute Frequency in Korea-related ", input, " Articles")
## make a table
time.stamp <- unique(pd$time)
sumstat.list <-
    lapply(1:length(time.stamp), function(t){
        pd[pd$time == time.stamp[t], ] %>% 
            group_by(ngram) %>%
            summarize(n = sum(n_month))%>%
            arrange(desc(n))
    })
sumstat.list <- lapply(1:length(time.stamp), function(t){sumstat.list[[t]][1: cut.point, ]})
dt <- Reduce(cbind, sumstat.list)
colnames(dt) <- unlist(lapply(1:length(time.stamp), function(t){c(paste0(time.stamp[t], " : Word"),"Freq")}))
kable(dt, caption = title, digits=2) %>% kable_styling(font_size = 10, full_width=FALSE) %>%
    save_kable(file = paste0(file.name, "_biigram_absolute.html"), self_contained = T)

p1 <- ggplot(pd, aes(order, n_month, fill = factor(month))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ time, ncol = 3, scales = "free") + theme_grey(base_size = 22) + 
    labs(title=title, subtitle = subtitle, y = "Absolute Frequency", x="Month", 
         caption = "Copyright: SNU IIS Global Data Center") +
    scale_x_continuous(
        breaks = pd$order,
        labels = pd$ngram,
        expand = c(0,0)
    ) +
    coord_flip()
pdf(file=paste0(file.name, "_topcut.pointbigram_absolute.pdf"),
    family="sans",width=16, height=15)
print(p1)
dev.off()
png(file=paste0(file.name, "_topcut.pointbigram_absolute.png"),
    width = 405, height = 365, units='mm', res = 150)
print(p1)
dev.off()
#########################
### bigram relative frequency
#########################
topcut.point <- input_bigrams_by_article %>%
    dplyr::select(ngram,year,month,n_month,tf_idf_month) %>%
    distinct() %>%
    filter(!ngram %in% na_bigrams) %>%
    group_by(year,month) %>%
    top_n(cut.point, wt=jitter(tf_idf_month)) %>%
    arrange(year,month,-tf_idf_month) %>%
    mutate(time = paste(year, month, sep="-"))

pd <- topcut.point %>%
    group_by(month) %>%
    ungroup() %>%
    arrange(month, tf_idf_month) %>%
    mutate(order = row_number())


p11 <- ggplot(pd, aes(order, tf_idf_month, fill = factor(month))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ time, ncol = 3, scales = "free") + theme_grey(base_size = 22) + 
    labs(title=title, subtitle = subtitle, y = "Frequency", x="Month", 
         caption = "Copyright: SNU IIS Global Data Center") +
    scale_x_continuous(
        breaks = pd$order,
        labels = pd$ngram,
        expand = c(0,0)
    ) +
    coord_flip()


title = paste0("Bigram Relative Frequency in Korea-related ", input, " Articles") ;
## make a table
time.stamp <- unique(pd$time)
sumstat.list <-
    lapply(1:length(time.stamp), function(t){
        pd[pd$time == time.stamp[t], ] %>% 
            group_by(ngram) %>%
            summarize(n = sum(tf_idf_month))%>%
            arrange(desc(n))
    })
sumstat.list <- lapply(1:length(time.stamp), function(t){sumstat.list[[t]][1: cut.point, ]})
dt <- Reduce(cbind, sumstat.list)
colnames(dt) <- unlist(lapply(1:length(time.stamp), function(t){c(paste0(time.stamp[t], " : Word"),"Freq")}))
kable(dt, caption = title, digits=2) %>% kable_styling(font_size = 10, full_width=FALSE) %>%
    save_kable(file = paste0(file.name, "_biigram_relative.html"), self_contained = T)

p12 <- ggplot(pd, aes(order, tf_idf_month, fill = factor(month))) +
   geom_col(show.legend = FALSE) +
    facet_wrap(~ time, ncol = 3, scales = "free") + theme_grey(base_size = 22) + 
    labs(title=title, subtitle = subtitle, y = "Frequency", x="Bigram Word", 
         caption = "Copyright: SNU IIS Global Data Center") +
    scale_x_continuous(
        breaks = pd$order,
        labels = pd$ngram,
        expand = c(0,0)
    ) +
        coord_flip()


## library(gridExtra)
pdf(file=paste0(file.name, "_topcut.pointbigram_relative.pdf"),
    family="sans", width=16, height=15)
print(p12)
 ## do.call(grid.arrange, c(p.list, nrow=2))
dev.off()
png(file=paste0(file.name, "_topcut.pointbigram_relative.png"),
    width = 405, height = 365, units='mm', res = 150)
print(p12)
dev.off()
