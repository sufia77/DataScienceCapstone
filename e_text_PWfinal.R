#=====This section is to test locally======
setwd("c:/capstone6")

library(stringr)
library(NLP)
library(tm)

final5GTable <- readRDS("final5GTable.RDS")
final4GTable <- readRDS("final4GTable.RDS")
final3GTable <- readRDS("final3GTable.RDS")
final2GTable <- readRDS("final2GTable.RDS")

# txt <- "try To get @ a bachelor's degree in"
# txt <- "a bag"
# txt <- "A nasty"
# txt <- "asked which"
# txt <- "Last of"
# txt <- "Hey! are"
# txt <- "No"
# txt <- "Best   "
# txt <- "last"
# txt <- "Mike"
# txt <- "jack"
# txt <- "hackraxy"
# txt <- ""
# txt <- ""
# sentence<- txt
predictNextWord6(txt)
#============================================
#############################################
predictNextWord6 <- function(sentence){

  # ---------------- clean text-----------------------
  sentence <- gsub("\\b\\S+\\@\\S+\\..{1,13}(\\s)?\\b)", " ", sentence)      # removeS email address
  sentence <- gsub("#\\w+", " ", sentence)                                   # removes a word after # sign. e.g. #this
  sentence <- gsub("@\\w+", " ", sentence)                                   # removes a word after @ sign. e.g. @this
  sentence <- gsub("(http|ftp|file)(\\w)?\\://[^[:space:]]*", " ", sentence) # removeS http*, ftp* and file* URLS
  sentence <- gsub("www\\.[^[:space:]]+", " ", sentence)                     # removeS www website not started with http
  sentence <- gsub("/|@|\\|", " ", sentence)                                 # removes these 3 characters: / @ |
  sentence <- gsub("[[:digit:]]+\\.[[:digit:]]+", " ", sentence)             # removes decimal numbers so dots(.) are not left as punctuation after removenumbers()                
  sentence <- removeNumbers(sentence)                   # Is it necessary?   # removes all digits (0-9)
  sentence <- gsub("[^[:graph:]]", " ", sentence)                            # removeS all except [:alnum:] and [:punct:]
  sentence <- gsub("[[:blank:]]+", " ", sentence)                            # replaces multiple spaces, tabs, with a single space
  #...........................................
  sentence <- gsub("'", "aazzbbyyccxx", sentence)                            # rreserves appostrophe before removing punctuation
  #...........................................
  sentence <- gsub(",", ", ", sentence)
  sentence <- removePunctuation(sentence, preserve_intra_word_dashes = TRUE)
  sentence <- gsub("-", " ", sentence)
  #...........................................
  sentence <- gsub("aazzbbyyccxx", "'", sentence)                            # rreserves appostrophe before removing punctuation
  #...........................................              
  sentence <- gsub("\\w{30,}", "", sentence)                                 # remove words wiht 30 characters or longer
  sentence <- gsub("[[:blank:]]+", " ", sentence)                            # again,replaces multiple spaces, tabs, with a single space
  sentence <- tolower(sentence)
  #--------------------------------------------------------
  
  searchTerm<- tail(strsplit(sentence,split="\\s+")[[1]],4)
  n<- length(searchTerm)
  searchTerm<- str_c(searchTerm,collapse=" ")
  
  w4<-w3<-w2<-w1<-wx<- character(0)
  
  if(n>=1) { w1<- tail(strsplit(searchTerm,split=" ")[[1]],1) }
  if(n>=2) { w2<- tail(strsplit(searchTerm,split=" ")[[1]],2) }
  if(n>=3) { w3<- tail(strsplit(searchTerm,split=" ")[[1]],3) }
  if(n>=4) { w4<- tail(strsplit(searchTerm,split=" ")[[1]],4) }
  
  w1<- str_c(w1,collapse=" ")
  w2<- str_c(w2,collapse=" ")
  w3<- str_c(w3,collapse=" ")
  w4<- str_c(w4,collapse=" ")
  w1; w2; w3; w4; wx
  
  if(length(wx)<3 & n>=4){
    tx <- final5GTable[final5GTable$firstTerm == w4, ]
    if (nrow(tx) > 0){
      wx <- unique(c(wx,strsplit(as.character(tx$lastTerm[1])," ")[[1]]))
    }
  }
  
  if(length(wx)<3 & n>=3 ){
    tx <- final4GTable[final4GTable$firstTerm == w3, ]
    if (nrow(tx) > 0){
      wx <- unique(c(wx,strsplit(as.character(tx$lastTerm[1])," ")[[1]]))
    }
  }
  
  if(length(wx)<3 & n>=2){
    tx <- final3GTable[final3GTable$firstTerm == w2, ]
    if (nrow(tx) > 0){
      wx <- unique(c(wx,strsplit(as.character(tx$lastTerm[1])," ")[[1]]))
    }
  }
  
  if(length(wx)<3 & n>=1){
    tx <- final2GTable[final2GTable$firstTerm == w1, ]
    if (nrow(tx) > 0){
      wx <- unique(c(wx,strsplit(as.character(tx$lastTerm[1])," ")[[1]]))
    }
  }
  
  if(length(wx)==1){ wx<- c(wx, "", "") }
  if(length(wx)==2){ wx<- c(wx, "") }
  if(length(wx)>3){ wx<- wx[1:3] }
  
  print(wx)
  
  # -------------------------------------------
  wxlen<- length(wx)
  andloc<- grep("^and$", wx)
 
  if(length(andloc)>0 & wxlen>0){
    if(andloc==1 & wxlen==2){
      wx<- wx[c(2,1)]
    }
    if(andloc==1 & wxlen==3){
      wx<- wx[c(2,3,1)]
    }
    if(andloc==2 & wxlen==3){
      wx<- wx[c(1,3,2)]
    }
  }
  
  # -------------------------------------------
  
  return(wx)
}
#################################################################





