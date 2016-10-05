predictNextWord6 <- function(sentence){
  
  library(NLP)
  library(tm)
  # clean text
  sentence <- gsub("\\b\\S+\\@\\S+\\..{1,3}(\\s)?\\b)", " ", sentence)
  sentence <- gsub("#\\w+", " ", sentence)
  sentence <- gsub("@\\w+", " ", sentence)
  sentence <- gsub("http[^[:space:]]*", " ", sentence)
  sentence <- gsub("/|@|\\|", " ", sentence)
  #sentence <- gsub("\\s+", " ", sentence)
  sentence <- removePunctuation(sentence)
  sentence <- removeNumbers(sentence)
  sentence <- gsub("[^[:graph:]]", " ", sentence)
  sentence <- gsub("\"", "", sentence)
  sentence <- tolower(sentence)
  sentence<- gsub("\\s+", " ", str_trim(sentence))
  
  library(stringr)
  
  w4<-w3<-w2<-w1<- ""
  
  last_words<- tail(strsplit(sentence,split="\\s+")[[1]],4)
  n<- length(last_words)
  last_words<- str_c(last_words,collapse=" ")
  
  if(n>=1) { w1<- tail(strsplit(last_words,split=" ")[[1]],1) }
  if(n>=2) { w2<- tail(strsplit(last_words,split=" ")[[1]],2) }
  if(n>=3) { w3<- tail(strsplit(last_words,split=" ")[[1]],3) }
  if(n>=4) { w4<- tail(strsplit(last_words,split=" ")[[1]],4) }
  
  w1<- str_c(w1,collapse=" ")
  w2<- str_c(w2,collapse=" ")
  w3<- str_c(w3,collapse=" ")
  w4<- str_c(w4,collapse=" ")
  
  # w1; w2; w3; w4
  
  if(w4 %in% fiveGTable$firstTerm){
    t1 <- fiveGTable[fiveGTable$firstTerm == w4, ]
    p1 <- t1$lastTerm[1:3]
    wx <- p1[!is.na(p1)]
    
    if(length(wx) < 3){
      t2 <- fourGTable[fourGTable$firstTerm == w3, ]
      p2 <- t2$lastTerm[1:3]
      w3x <- p2[!is.na(p2)]
      wx <- unique(c(wx, w3x))
      
      if(length(wx)<3){
        t3 <- threeGTable[threeGTable$firstTerm == w2, ]
        p3 <- t3$lastTerm[1:3]
        w2x <- p3[!is.na(p3)]
        wx <- unique(c(wx, w2x))
        
        if(length(wx)<3){
          t4 <- twoGTable[twoGTable$firstTerm == w1, ]
          p4 <- t4$lastTerm[1:3]
          w1x <- p4[!is.na(p4)]
          wx <- unique(c(wx, w1x)) 
        }
      }
    }
  }else 
    if(w3 %in% fourGTable$firstTerm){
      t1 <- fourGTable[fourGTable$firstTerm == w3, ]
      p1 <- t1$lastTerm[1:3]
      wx <- p1[!is.na(p1)]
      
      if(length(wx) < 3){
        t3 <- threeGTable[threeGTable$firstTerm == w2, ]
        p3 <- t3$lastTerm[1:3]
        w2x <- p3[!is.na(p3)]
        wx <- unique(c(wx, w2x))
        
        if(length(wx)<3){
          t4 <- twoGTable[twoGTable$firstTerm == w1, ]
          p4 <- t4$lastTerm[1:3]
          w1x <- p4[!is.na(p4)]
          wx <- unique(c(wx, w1x)) 
        }
      }
    }else 
      if(w2 %in% threeGTable$firstTerm){
        t1 <- threeGTable[threeGTable$firstTerm == w2, ]
        p1 <- t1$lastTerm[1:3]
        wx <- p1[!is.na(p1)]
        if(length(wx) < 3){
          t4 <- twoGTable[twoGTable$firstTerm == w1, ]
          p4 <- t4$lastTerm[1:3]
          w1x <- p4[!is.na(p4)]
          wx <- unique(c(wx, w1x)) 
        }
      } else 
        if(w1 %in% twoGTable$firstTerm){
          t1 <- twoGTable[twoGTable$firstTerm == w1, ] 
          p1 <- t1$lastTerm[1:3]
          wx <- p1[!is.na(p1)]
        } else 
        { print("word not in the ngrams") }
  
  wx[1:3]
}
