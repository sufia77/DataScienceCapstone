---
title: "Milestone Report - Data Science Capston Project"
author: "Sufia Khatun"
date: "August 31, 2016"
output: html_document
---



## Synopsis

In this report, the Natural Language Processing was studied by analyzing the text documents provided by SwiftKey. The text documents were provided in four different languages. This study focused on the text documents in english language that were collected from three different sources: twitter, news and blogs.

The goal of this report was to process the text data, perform exploratory analysis, understand variation in the frequencies of words and word pairs in the data and prepare the text data to build model for predictive text mining application.

Dataset was downloaded from Coursera provided link: [Dataset]("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip")

## R Package Libraries Used in this Study


```r
# Necessary libraries for NLP
#library(rJava)
library(NLP) #Generics NLP Function set
library(openNLP)
library(ggplot2)
library(tm) #For Text Mining & Corpus workings. 
library(wordcloud) # for wordcloud
library(quanteda) # for ngrams
library(magrittr)
library(knitr) # for table
```

## Loading and Sampling Dataset

Three text files: twitter, news and blogs were downloaded from the above mentioned link and then saved in the current working directory. Three files were combined into one file so that random lines could be chosen from all three files.


```r
## Download and unzip the data file
# fileUrl <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
# destfile <- file.path(getwd(), "./project/Dataset.zip")
# download.file(fileUrl, destfile, method = "curl")
# unzip(zipfile = "./project/Dataset.zip", exdir = "./project")
file_twitter <- readLines("./project/final/en_US/en_US.twitter.txt",encoding = "UTF-8-mac", skipNul = TRUE)
file_news <- readLines("./project/final/en_US/en_US.news.txt", encoding = "UTF-8-mac", skipNul = TRUE)
file_blogs <- readLines("./project/final/en_US/en_US.blogs.txt", encoding = "UTF-8-mac", skipNul = TRUE)
data_all <- c(file_twitter, file_news, file_blogs)
```

## Summary of Dataset

To visualize the summary of the text files: number of lines, maximum numbers of character, total number of words,  mean and standard deviation of words in each file were calculated. 


|        | Total_Lines| Max_Char| Total_Words| Mean_Words| SD_Words|
|:-------|-----------:|--------:|-----------:|----------:|--------:|
|Twitter |     2360148|      140|    30373605|   12.86936|  6.90375|
|News    |     1010242|    11384|    34372814|   34.02434| 22.59018|
|Blogs   |      899288|    40833|    37334149|   41.51523| 46.27129|


## Sampling the dataset

Considering the memory of the processing computer and large text documents, ten thousands random lines were chosen from the combined document for further analysis.


```r
set.seed(150)
sample_all <- sample(data_all, 10000)
# remove(file_twitter,file_blogs, file_news)
```

## Data Processing
### Profanity filter
To process the text data,  uninteresting words were remove using "tm_map" function. Also, Bad words were remove from the text data which were found from this link: [BadWords]("http://www.cs.cmu.edu/~biglou/resources/bad-words.txt").


```r
##============Tokenization and Profanity Filter============================
# Removing bad words found from "http://www.cs.cmu.edu/~biglou/resources/bad-words.txt"
bwds <- readLines("./bad-words.txt")
sample_text <- removeWords(sample_all, bwds)

# Creating corpora of text data with each line as a vector 
corp_sample <- Corpus(VectorSource(sample_all))

# Removing punctuation, numbers, stopwords, stemming, extra whitespace.
corp_sample <- tm_map(corp_sample, removePunctuation)
corp_sample <- tm_map(corp_sample, removeNumbers)
corp_sample <- tm_map(corp_sample, content_transformer(tolower))
corp_sample <- tm_map(corp_sample, removeWords, stopwords("english"))
corp_sample <- tm_map(corp_sample, stemDocument)
corp_sample <- tm_map(corp_sample, stripWhitespace)
corp_sample <- tm_map(corp_sample, PlainTextDocument)
# This is the end of preprocessing stage

inspect(corp_sample[1])
```

```
## <<VCorpus>>
## Metadata:  corpus specific: 0, document level (indexed): 0
## Content:  documents: 1
## 
## [[1]]
## <<PlainTextDocument>>
## Metadata:  7
## Content:  chars: 43
```

## Exploratory Analysis

To explore data, first a matrix was created with the words from corpus data then sorted the words with respect to the frequency of words in the data. To vizualize, a wordcloud was created with top 100 frequent words. 


```r
# ============ Explore the data =========================
tdmUS <- removeSparseTerms(TermDocumentMatrix(corp_sample), 0.9999)
# ============ Obtain words and their frequencies==============
# define tdm as matrix
m = as.matrix(tdmUS)
# get word counts in decreasing order
word_freqs = sort(rowSums(m), decreasing=TRUE)
dfUS <- data.frame(word=names(word_freqs), freq = word_freqs)
wordcloud(names(word_freqs), word_freqs, max.words = 100,random.order=FALSE, rot.per = 0.3, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

## Tokenize and Build of n-grams Model

To understand the distribution of words and relation between the words in the corpora, the dataframes with unigram, bigram and trigram were created. To obtain unigram, bigram and trigram, "quandeta" package was used. This package was found simpler and faster compare to "RWeka". Top 10 highly used words and word pairs in this language was shown below.


```r
#=============================
# require(quanteda)
qcorpus <- corpus(sample_all)

ngramFun<-function(qcn,i)
{
  uni_gram <- dfm(qcn, ngrams=i, concatenator = " ", ignoredFeatures=c("will", stopwords("english")), verbose = FALSE)
  df_dfm <- data.frame(word = features(uni_gram), freq = colSums(uni_gram), row.names = NULL, stringsAsFactors = FALSE)
  dfn <- df_dfm[order(-df_dfm$freq), ]
}

df_unigram <- ngramFun(qcorpus,1)
df_bigram <- ngramFun(qcorpus,2)
df_trigram <- ngramFun(qcorpus,3)

head(df_unigram)
```

```
##     word freq
## 68  just  716
## 154 said  707
## 520  one  691
## 130 like  612
## 278  can  601
## 141 time  567
```

```r
head(df_bigram)
```

```
##            word freq
## 838   right now   49
## 685   last year   48
## 597  last night   40
## 1905   new york   40
## 469   years ago   34
## 564   last week   31
```

```r
head(df_trigram)
```

```
##                 word freq
## 1295    world war ii    9
## 2416     let us know    8
## 854   four years ago    5
## 857    new york city    5
## 2690 st louis county    5
## 4357   just got home    5
```

## Plot for Unigram, Bigram and Trigram


```r
ngramPlot <- function(data, title) {
  ggplot(data[1:20,], aes(reorder(word, -freq), freq))+
    geom_bar(stat = "identity",fill="light Blue")+
    theme(axis.text.x = element_text(angle = 45, size = 12, hjust = 1)) +
    geom_text(aes(label=freq), vjust = - 0.20, size = 3)+labs(x = "Word/Word Pairs", y = "Frequency") +
    ggtitle(title)
}
ngramPlot(df_unigram, "Top 20 Unigrams")
```

![plot of chunk plot](figure/plot-1.png)

```r
ngramPlot(df_bigram, "Top 20 Bigrams")
```

![plot of chunk plot](figure/plot-2.png)

```r
ngramPlot(df_trigram, "Top 20 Triigrams")
```

![plot of chunk plot](figure/plot-3.png)

## How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%? 

Following is the function to find the unique words that occurs 50% and 90% of the total words in the language. First I calculated the the total number of words and how many times each word was repeated. Then make a condition to find unique words. 


```r
uniqueWord <- function(df, coverage){
  sum_freq <- 0
  coverage_freq <- coverage*sum(df$freq)
  for (i in 1:nrow(dfUS)) {
    sum_freq <- sum_freq + df[i,]$freq
    if(sum_freq >= coverage_freq){
      break
    }
  }
  i
}

uniqueWord(dfUS, 0.5) # Unique words with 50% coverage
```

```
## [1] 572
```

```r
uniqueWord(dfUS, 0.9) # Unique words with 90% coverage
```

```
## [1] 6318
```

## Plot for Unique Words Coverage

A graph was created to see the trends of unique words with respect to the percentage coverage.


```r
Percent_Coverage <- c(10,30,50,70,90)
Unique_Words <- c(uniqueWord(dfUS,0.1), uniqueWord(dfUS,0.3), uniqueWord(dfUS,0.5), uniqueWord(dfUS,0.7), uniqueWord(dfUS, 0.9))
df1 <- data.frame(Percent_Coverage, Unique_Words)
ggplot(df1, aes(x=Percent_Coverage, y=Unique_Words))+geom_line() +
  geom_text(aes(label=Unique_Words), hjust=1.35, vjust=-0.1)+
  ggtitle("Unique Words vs Percent Coverage")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png)

## How do you evaluate how many of the words come from foreign languages? 

To find the answer of this question, I need to make a dictionary with english words only then remove these words from the text documents. The sum of the remaining words will be the words came from foreign languages.

## Future Study

* Analyze two, three and four grams to build model to predict next word.
* Make a shiny app.

