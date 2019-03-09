##################################################
## functions to execute plotting codes
## Run this after keyword_*****.R
##################################################


#########################
## top 40 uniigram plot
#########################
## absolute frequency
top40 <- input_unigrams_by_article %>%
    select(ngram,year,month,n_month,tf_idf_month) %>%
    distinct() %>%
    filter(!ngram %in% na_bigrams) %>%
    group_by(year,month) %>%
    top_n(40, wt=n_month) %>%
    arrange(year,month,-n_month) %>%
    mutate(time = paste(year, month, sep="-"))

pd <- top40 %>%
    group_by(month) %>%
    ungroup() %>%
    arrange(month, n_month) %>%
    mutate(order = row_number())

title = paste0("Unigram Word Frequency in Korea-related ", input, " Articles")

p01 <- ggplot(pd, aes(order, n_month, fill = factor(month))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ time, ncol = 4, scales = "free") +
    labs(title=title, subtitle = subtitle, y = "Absolute Frequency", x="Month", 
         caption = "Copyright: SNU IIS Global Data Center") +
    scale_x_continuous(
        breaks = pd$order,
        labels = pd$ngram,
        expand = c(0,0)
    ) +
    coord_flip()

pdf(file=paste0(file.name, "_top40unigram_absolute.pdf"),
    family="sans", width=16, height=15)
p01
dev.off()

## relative frequency
top40 <- input_unigrams_by_article %>%
    select(ngram,year,month,n_month,tf_idf_month) %>%
    distinct() %>%
    filter(!ngram %in% na_bigrams) %>%
    group_by(year,month) %>%
    top_n(40, wt=tf_idf_month) %>%
    arrange(year,month,-tf_idf_month) %>%
    mutate(time = paste(year, month, sep="-"))

pd <- top40 %>%
    group_by(month) %>%
    ungroup() %>%
    arrange(month, tf_idf_month) %>%
    ## 3. Add order column of row numbers
    mutate(order = row_number())

title = paste0("Unigram Word Frequency in Korea-related ", input, " Articles")

p02 <- ggplot(pd, aes(order, tf_idf_month, fill = factor(month))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ time, ncol = 4, scales = "free") +
    labs(title=title, subtitle = subtitle, y = "Relative Frequency", x="Month", 
         caption = "Copyright: SNU IIS Global Data Center") +
    scale_x_continuous(
        breaks = pd$order,
        labels = pd$ngram,
        expand = c(0,0)
    ) +
    coord_flip()
pdf(file=paste0(file.name, "_top40unigram_relative.pdf"),
    family="sans", width=16, height=15)
p02
dev.off()


#########################
## top 40 bigram plot
#########################
top40 <- input_bigrams_by_article %>%
    select(ngram,year,month,n_month,tf_idf_month) %>%
    distinct() %>%
    filter(!ngram %in% na_bigrams) %>%
    group_by(year,month) %>%
    top_n(20, wt=n_month) %>%
    arrange(year,month,-n_month) %>%
    mutate(time = paste(year, month, sep="-"))

## because of facetting, ordering is not working.
## So, we have to ungroup it and name a new variable "order"
pd <- top40 %>%
    group_by(month) %>%
    ungroup() %>%
    arrange(month, n_month) %>%
    mutate(order = row_number())

title = paste0("Bigram Word Frequency in Korea-related ", input, " Articles")
p1 <- ggplot(pd, aes(order, n_month, fill = factor(month))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ time, ncol = 4, scales = "free") +
    labs(title=title, subtitle = subtitle, y = "Absolute Frequency", x="Month", 
         caption = "Copyright: SNU IIS Global Data Center") +
    scale_x_continuous(
        breaks = pd$order,
        labels = pd$ngram,
        expand = c(0,0)
    ) +
    coord_flip()
pdf(file=paste0(file.name, "_top40bigram_absolute.pdf"),
    family="sans",width=16, height=15)
p1
dev.off()

#########################
### top 40 bigram relative frequency
#########################
top40 <- input_bigrams_by_article %>%
    select(ngram,year,month,n_month,tf_idf_month) %>%
    distinct() %>%
    filter(!ngram %in% na_bigrams) %>%
    group_by(year,month) %>%
    top_n(40, wt=tf_idf_month) %>%
    arrange(year,month,-tf_idf_month) %>%
    mutate(time = paste(year, month, sep="-"))

pd <- top40 %>%
    group_by(month) %>%
    ungroup() %>%
    arrange(month, tf_idf_month) %>%
    mutate(order = row_number())


p11 <- ggplot(pd, aes(order, tf_idf_month, fill = factor(month))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ time, ncol = 4, scales = "free") +
    labs(title=title, subtitle = subtitle, y = "Frequency", x="Month", 
         caption = "Copyright: SNU IIS Global Data Center") +
    scale_x_continuous(
        breaks = pd$order,
        labels = pd$ngram,
        expand = c(0,0)
    ) +
    coord_flip()


title = paste0("Bigram Word Frequency in Korea-related ", input, " Articles") ;
p12 <- ggplot(pd, aes(order, tf_idf_month, fill = factor(month))) +
   geom_col(show.legend = FALSE) +
    facet_wrap(~ time, ncol = 4, scales = "free") +
    labs(title=title, subtitle = subtitle, y = "Frequency", x="Bigram Word", 
         caption = "Copyright: SNU IIS Global Data Center") +
    scale_x_continuous(
        breaks = pd$order,
        labels = pd$ngram,
        expand = c(0,0)
    ) +
        coord_flip()


## library(gridExtra)
pdf(file=paste0(file.name, "_top40bigram_relative.pdf"),
    family="sans", width=16, height=15)
p12 ## do.call(grid.arrange, c(p.list, nrow=2))
dev.off()

