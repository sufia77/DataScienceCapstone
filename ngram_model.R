projecthome <- "c:/capstone2"
setwd(projecthome)

twitterfile <- paste0(projecthome, "/project/final/en_us/", "en_US.twitter.txt")
newsfile    <- paste0(projecthome, "/project/final/en_us/", "en_US.news.txt")
blogsfile   <- paste0(projecthome, "/project/final/en_us/", "en_US.blogs.txt")
badwordfile <- paste0(projecthome, "/", "bad-words.txt")

text_twitter <- readLines(twitterfile, encoding = "UTF-8", skipNul = TRUE)
text_news    <- readLines(newsfile, encoding = "UTF-8", skipNul = TRUE)
text_blogs   <- readLines(blogsfile, encoding = "UTF-8", skipNul = TRUE)
bwds         <- readLines(badwordfile)


# install.packages("NLP")
# install.packages("tm")
# install.packages("quanteda")
library(NLP)
library(tm)

#set.seed(1500)
text_all <- c(text_twitter, text_news, text_blogs)


text_all <- gsub("\\b\\S+\\@\\S+\\..{1,3}(\\s)?\\b)", " ", text_all)
text_all <- gsub("#\\w+", " ", text_all)
text_all <- gsub("@\\w+", " ", text_all)
text_all <- gsub("http[^[:space:]]*", " ", text_all)
text_all <- gsub("/|@|\\|", " ", text_all)
text_all <- removePunctuation(text_all)
text_all <- removeNumbers(text_all)
text_all <- gsub("[^[:graph:]]", " ", text_all)
text_all <- tolower(text_all)
text_all <- removeWords(text_all, bwds)
text_all <- gsub("\\s+", " ", text_all)

writeLines(text_all,"text_all.txt")
# text_all<- readLines("text_all.txt", encoding = "UTF-8", skipNul = TRUE)
library(rJava)
library(quanteda)

qcorpus <- corpus(text_all)

ngramFun <- function(qc, i){
  n_gram <- dfm(qc, ngrams = i, concatenator = " ", verbose = FALSE)
  ngram_df <- data.frame(words = features(n_gram), freq = colSums(n_gram), row.names = NULL, stringsAsFactors = FALSE)
  dfn <- ngram_df[order(-ngram_df$freq), ]
}

twogram <- ngramFun(qcorpus, 2)
save(twogram, file = "twogram.RData")

threegram <- ngramFun(qcorpus, 3)
save(threegram, file = "threegram.RData")

fourgram <- ngramFun(qcorpus, 4)
save(fourgram, file = "fourgram.RData")

fivegram <- ngramFun(qcorpus, 5)
save(fivegram, file = "fivegram.RData")

#load("twogram.RData")
#load("threegram.RData")
#load("fourgram.RData")
#load("fivegram.RData")

fiveG <- fivegram[fivegram$freq >5, ]
save(fiveG, file = "fiveG.RData")

fourG <- fourgram[fourgram$freq >5, ]
save(fourG, file = "fourG.RData")

threeG <- threegram[threegram$freq >4, ]
save(threeG, file = "threeG.RData")

#Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7') # for 64-bit version
library(qdap)
library(dplyr)
library(tidyr)

# Twogram table
df1 <- colsplit2df(twogram, splitcols = 1 , sep = " ")

twoGTable <- rename(df1, firstTerm = X1, lastTerm = X2)

twoGTable1 <- twoGTable[twoGTable$freq > 1, ]
saveRDS(twoGTable, file = "twoGTable.rds")

save(twoGTable1, file = "twoGTable1.RData")
saveRDS(twoGTable1, file = "twoGTable1.rds")

# Threegram table
df2 <- colsplit2df(threeG, sep = " ")

df2 <- df2 %>%
  unite(firstTerm, X1, X2, sep = " ",remove = FALSE) 

threeGTable <- rename(df2, lastTerm = X3) %>%
  select(firstTerm, lastTerm, freq)

save(threeGTable, file = "threeGTable.RData")
saveRDS(threeGTable, file = "threeGTable.rds")

# Fourgram table
df3 <- colsplit2df(fourG, sep = " ")

df3 <- df3 %>%
  unite(firstTerm, X1, X2,X3, sep = " " ,remove = FALSE) 

fourGTable <- rename(df3, lastTerm = X4) %>%
  select(firstTerm, lastTerm, freq)

save(fourGTable, file = "fourGTable.RData")
saveRDS(fourGTable, file = "fourGTable.rds")

# Fivegram table
df4 <- colsplit2df(fiveG, sep = " ")

df4 <- df4 %>%
  unite(firstTerm, X1, X2,X3,X4, sep = " ", remove = FALSE) 

fiveGTable <- rename(df4, lastTerm = X5) %>%
  select(firstTerm, lastTerm, freq)

save(fiveGTable, file = "fiveGTable.RData")
saveRDS(fiveGTable, file = "fiveGTable.rds")
