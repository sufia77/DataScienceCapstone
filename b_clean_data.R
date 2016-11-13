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

twitterfile <- paste0(datafolder, "/en_US.twitter.txt")
newsfile    <- paste0(datafolder, "/en_US.news.txt")
blogsfile   <- paste0(datafolder, "/en_US.blogs.txt")
badwordfile <- paste0(datafolder, "/bad-words.txt")

text_twitter <- readLines(twitterfile, encoding = "UTF-8", skipNul = TRUE, n=-1)
text_news    <- readLines(newsfile, encoding = "UTF-8", skipNul = TRUE, n=-1)
text_blogs   <- readLines(blogsfile, encoding = "UTF-8", skipNul = TRUE, n=-1)
text_twitter <- text_twitter[1:round(length(text_twitter)*samplesizepct/100)]
text_news    <- text_news[1:round(length(text_news)*samplesizepct/100)]
text_blogs   <- text_blogs[1:round(length(text_blogs)*samplesizepct/100)]
text_all     <- c(text_blogs, text_twitter, text_news)
bwds         <- readLines(badwordfile)

remove(text_twitter)
remove(text_news)
remove(text_blogs)
#==================================================
text_all <- gsub("\\b\\S+\\@\\S+\\..{1,13}(\\s)?\\b)", " ", text_all)      # removeS email address with domain names up to 13 character long
text_all <- gsub("#\\w+", " ", text_all)                                   # removes a word after # sign. e.g. #this
text_all <- gsub("@\\w+", " ", text_all)                                   # removes a word after @ sign. e.g. @this
text_all <- gsub("(http|ftp|file)(\\w)?\\://[^[:space:]]*", " ", text_all) # removeS http*, ftp* and file* URLS
text_all <- gsub("www\\.[^[:space:]]+", " ", text_all)                     # removeS www website not started with http
text_all <- gsub("/|@|\\|", " ", text_all)                                 # removes these 3 characters: / @ |
text_all <- gsub("[[:digit:]]+\\.[[:digit:]]+", " ", text_all)             # removes decimal numbers so dots(.) are not left as punctuation after removenumbers()                
text_all <- removeNumbers(text_all)                   # Is it necessary?   # removes all digits (0-9)
text_all <- gsub("[^[:graph:]]", " ", text_all)                            # removeS all except [:alnum:] and [:punct:]
text_all <- gsub("[[:blank:]]+", " ", text_all)                            # replaces multiple spaces, tabs, with a single space
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
text_all <- badword2period(text_all, bwds)                                 # replaces badwords with period (.)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############################################
text_all <- makesentence(text_all,1000)                                    # convert lines to sentences number at a chunk
############################################
#...........................................
text_all <- gsub("'", "aazzbbyyccxx", text_all)                            # reserves appostrophe placeholders before removing punctuation
#...........................................
text_all <- gsub(",", ", ", text_all)
text_all <- removePunctuation(text_all, preserve_intra_word_dashes = TRUE)
text_all <- gsub("-", " ", text_all)
#...........................................
text_all <- gsub("aazzbbyyccxx", "'", text_all)                            # reserves appostrophe before removing punctuation
#...........................................              
text_all <- gsub("\\w{30,}", "", text_all)                                 # remove words wiht 30 characters or longer
text_all <- gsub("\n'|'\n", "\n", text_all)                                # removes trialing and ending appostrophe from a line
text_all <- gsub("^'", "", text_all)
text_all <- gsub("([[:blank:]]+'+)|('+[[:blank:]]+)", " ", text_all)       # removes trialing and ending appostrophe from a word

text_all <- gsub("[[:blank:]]+", " ", text_all)                            # again,replaces multiple spaces, tabs, with a single space
text_all <- gsub("\n[[:blank:]]+", "\n", text_all)                         # remove trialing blanks (spaces, tabs)
text_all <- gsub("\n\\s+", "\n", text_all)
text_all <- gsub("^\\s+", "", text_all)
text_all <- tolower(text_all)

writeLines(text_all,"text_all_final.txt")
########################################################

#=======================FUNCTIONS=======================
badword2period<- function(textlines, badwords){
  i<- 0
  total_badwords<- length(badwords)
  for(badword in badwords){
    i<- i+1
    x<- paste0('\\<', badword, '\\>')
    y<- '\\.'
    textlines<- gsub(x,y, textlines)
    if(i%%10==0 | i==total_badwords){ print(paste0(Sys.time(), ":: Completed ", i, " of ", total_badwords, " bad words replacement.")) }
  }
  return(textlines)
}
#-------------------------------------------------------
makesentence<- function(textlines,lineChunk){
  sentences<- NULL
  totalLines<- length(textlines)
  startLine<- 1
  endLine<- lineChunk
  
  while (endLine <= totalLines){
    strings<- as.String(textlines[(startLine):(endLine)])
    sentence_token_annotator <- Maxent_Sent_Token_Annotator(language = "en")
    sentence.boundaries <- annotate(strings, sentence_token_annotator)
    string_lines <- strings[sentence.boundaries]
    #string_lines <- gsub("\n+", "", string_lines)
    sentences<- c(sentences, string_lines)
    print(paste0(Sys.time(), ":: Converted ", endLine, " of ", totalLines, " lines to sentences."))
    
    remainingLines<- totalLines-endLine
    if((remainingLines > 0) & (remainingLines < lineChunk) ){ lineChunk<- remainingLines}
    startLine<- endLine+1;
    endLine<- endLine+lineChunk
  }
  return(sentences)
}
#=======================================================

