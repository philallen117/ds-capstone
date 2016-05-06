# sentences.R
source("util.R")
setJavaHeapSizeGB(4)
library(openNLP)
library(NLP)
library(data.table)
library(parallel)
options(mc.cores=6)

fileToSentences <- function(fname) {
  v <- readLines(fname, encoding="UTF-8")
  v <- iconv(v, "UTF-8", "ASCII//TRANSLIT", "")
  v <- gsub("\\\"", "", v)
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  sentences <- character()
  for (x in v) {
    x <- as.String(x)
    a <- annotate(x, sent_token_annotator)
    sentences <- c(sentences, x[a])
  }
  sentences
}

getAllSentencesRaw <- function() {
  news <- data.table(txt=fileToSentences("data/news.raw.txt"), corp="news")
  tweets <- data.table(txt=fileToSentences("data/tweets.raw.txt"), corp="tweets")
  blogs <- data.table(txt=fileToSentences("data/blogs.raw.txt"), corp="blogs")
  rbind(news, tweets, blogs)
}

news <- data.table(txt=fileToSentences("data/test.news.raw.txt"), corp="news")


# allSentencesRaw <- getAllSentencesRaw()
# save(allSentencesRaw, file="data/tmp.RData")

