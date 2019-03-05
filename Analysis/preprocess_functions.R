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