projecthome <- "c:/capstone7"
datafolder <- "./project/final/en_us"
setwd(projecthome)

samplesizepct<- 100     # % of total lines from each sample text

# install.packages("tm")
# install.packages("openNLP")
# install.packages("NLP")
library(tm)        # needed for removeNumbers()
library(openNLP)   # needed for Maxent_Sent_Token_Annotator()
library(NLP)       # automatically loaded with openNLP

# install.packages("quanteda")
library(quanteda)
#===============================================

text_all_final <- readLines("text_all_final.txt", encoding = "UTF-8", skipNul = TRUE, n=-1)
text_all_final <- text_all_final[1:round(length(text_all_final)*samplesizepct/100)]

#-------------------------------------------------
makeNGram <- function(qc, n, nameprefix, freqthreshold){
  nGramName<- paste0(nameprefix,"gram.RDS")
  nGName<- paste0(nameprefix,"G.RDS")
  
  n_gram <- dfm(qc, ngrams = n, concatenator = " ", verbose = FALSE)
  ngram_df <- data.frame(words = features(n_gram), freq = colSums(n_gram), row.names = NULL, stringsAsFactors = FALSE)
  
  ngram_df <- ngram_df[order(-ngram_df$freq), ]
  row.names(ngram_df)<- 1:nrow(ngram_df)
  nG <- ngram_df[ngram_df$freq > freqthreshold, ]
  row.names(nG)<- 1:nrow(nG)
  
  saveRDS(ngram_df, file = nGramName)
  saveRDS(nG, file = nGName)
  
  #return(nG)
}

qcorpus <- corpus(text_all_final)
remove(text_all_final)

makeNGram(qcorpus, 2, "two", 0)
makeNGram(qcorpus, 3, "three", 3)
makeNGram(qcorpus, 4, "four", 4)
makeNGram(qcorpus, 5, "five", 5)

remove(qcorpus)
#-------------------------------------------------
#=================================================

################################## DONE ####################################
