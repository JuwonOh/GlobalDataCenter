
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
                          layout = "fr", ## "nicely"
                          edge.col = "#FF0000",
                          text.col = "#35274A",
                          default.text.size = 1,
                          default.node.size= 5) {
    wordnetwork <- cooc_data %>% top_n(cut.point, wt=cooc)
    vert <- data.frame(node = names(table(c(wordnetwork$term1, wordnetwork$term2))),
                       size = as.vector(table(c(wordnetwork$term1, wordnetwork$term2))))

    wordnetwork2 <- igraph::graph_from_data_frame(wordnetwork, vertices = vert)
    plot <- ggraph(wordnetwork2, layout = "fr") +
        geom_edge_link(aes(edge_alpha = cooc), 
                       edge_colour = edge.col)  +
        geom_node_text(aes(label = name), col = text.col, size = 4*vert$size/max(vert$size) + default.text.size) +
        geom_node_point(size = 7*vert$size/max(vert$size) + default.node.size, col= "royalblue", alpha=0.2) +
        scale_size(range = c(4, 60), guide = 'none') + 
        theme_graph(base_family = "sans") +
        theme(legend.position = "none") +
        scale_edge_width(range = c(1, 2)) + scale_edge_alpha(range = c(0.2, 0.5)) +
        labs(title=title, subtitle = subtitle, caption = "Copyright: SNU IIS Global Data Center")
    return(plot)
}



prep_fun <- function(x) {
  # make text lower case
  x = str_to_lower(x)
  
  ## drop all numbers and symbols 
  x <- gsub("[]$*?[^{|\\#%&~_/<=>!,:;`\")(}@-]", " ", x)
  x <- gsub("[(\\.)'’“”—…–•]", " ", x)
  x <- gsub("[0-9]", " ", x)
  
  # collapse multiple spaces
  x = str_replace_all(x, "\\s+", " ")
  
  return(x)
}

prep_fun2 <- function(x) {
  ## often-used multigram
  x <- gsub("( na )|( news app )|( fox news )|( new york times )|( wall street journal )", " ", x)
  x <- gsub("kim( )?jung( )?un|kim( )?jong( )?un", "kimjongun", x)
  x <- gsub("(leader kimjongun )|(leader kim )", "kimjongun ", x)
  
  x <- gsub("moon( )?jae( )?in", "moonjaein", x)
  x <- gsub("(president moonjaein )|(president moon )", "moonjaein ", x)
  
  x <- gsub("white house", "whitehouse", x)
  x <- gsub("trump administration", "trumpadministration", x)
  
  x <- gsub("human rights", "humanrights", x)
  x <- gsub("north korea", "northkorea", x)
  x <- gsub("south korea", "southkorea", x)
  
  x <- gsub("national security", "nationalsecurity", x)
  x <- gsub("trade war", "tradewar", x)
  x <- gsub("rex tillerson", "rextillerson", x)
  x <- gsub("winter olympic", "winterolympic", x)
  
  x <- gsub("nuclear weapon", "nuclearweapon", x)
  x <- gsub("nuclear threat", "nuclearthreat", x)
  x <- gsub("nuclear button", "nuclearbutton", x)
  x <- gsub("nuclear attack", "nuclearattack", x)
  
  x <- gsub("nobel peace prize", "nobelpeaceprize", x)
  x <- gsub("foreign policy", "foreignpolicy", x)
  
  x <- gsub("secretary (state )?pompeo", "pompeo", x)
  
  x <- gsub("soviet union", "sovietunion", x)
  x <- gsub("donald trump", "trump", x)
  x <- gsub("president trump", "trump", x)
  x <- gsub("barack obama", "obama", x)
  x <- gsub("president obama", "obama", x)
  
  x <- gsub("united nations", "unitednations", x)
  x <- gsub("middle east", "middleeast", x)
  x <- gsub("kim( )?yo( )?jong", "kimyojong", x)
  x <- gsub("vladimir putin", "vladimirputin", x)
  x <- gsub("president vladimirputin", "vladimirputin", x)
  
  ## remove stopwords
  stopwords_regex <- paste(tm::stopwords('en'),
                           collapse = '\\b|\\b')
  x <- str_replace_all(x, stopwords_regex, '')
  
  ## drop a single letter word...
  x <- gsub("\\s*(?<!\\S)[a-zA-Z]{1}(?!\\S)", " ", x, perl=T)
  
  # collapse multiple spaces
  x = str_replace_all(x, "\\s+", " ")
  
  return(x)
}

na_bigrams <- c("email sign", "content programming", "copies content",
                "copyright network", "copyright notice", "displayed published",
                "distributed transmitted", "american people", "alter remove",
                "final form", "llc rights", "network llc", "notice copies",
                "prior written", "programming copyright", "protected united",
                "published broadcast","remove trademark","reproduced distributed",
                "reserved copyright","rush transcript","trademark copyright",
                "transmitted displayed","written permission","copyright law",
                "washington post", "york city", "ap photo", "rights reserved",
                "july copy", "begin video", "cq roll", "copyright cq", "permission cq",
                "video clip")

na_unigrams <- c("morning", "evening", "download", "news", "report",
                 "happy", "hour", "news", "brief", "briefing", "ap", "photos",
                 "day", "n.", "york", "wsj", "bret", "edt", "baier", "street", "wall",
                 "journal", "fox", "weekend", "quiz", "today", "briefe", "..",
                 "jae", NA, "video", "clip", "wall", "getelementbyid","www",
                 "abra")
