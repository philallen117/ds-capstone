# pre-proc.R
# setwd("C:/Users/Phil/repos/ds-capstone")
# cp raw data to data/samp1.* so pre.proc works.

pre.proc <- function (srce, rate) {
  fname <- paste("data/samp", rate, "en_US", srce, "txt", sep=".")
  v <- readLines(fname, encoding="UTF-8")
  v <- iconv(v, "UTF-8", "ASCII//TRANSLIT", "")
  v <- gsub("\\\"", "", v)
}

v.blogs.64 <- pre.proc("blogs", 8)
v.news.64 <- pre.proc("news", 8)
v.twitter.64 <- pre.proc("twitter", 8)
# v.blogs.8 <- pre.proc("blogs", 8)
# v.news.8 <- pre.proc("news", 8)
# v.twitter.8 <- pre.proc("twitter", 8)
# v.news.1 <- pre.proc("news", 8)
# v.blogs.1 <- pre.proc("blogs", 8)
# v.twitter.1 <- pre.proc("twitter", 8)

library(quanteda)


tok1.blogs.64 <- tokenize(v.blogs.64, removePunct=TRUE, removeNumbers=TRUE, ngrams=1)
dfm1.blogs.64 <- dfm(tok1.blogs.64) 
tok3.blogs.64 <- tokenize(v.blogs.64, removePunct=TRUE, removeNumbers=TRUE, ngrams=3)
dfm3.blogs.64 <- dfm(tok3.blogs.64) 
tok1.news.64 <- tokenize(v.news.64, removePunct=TRUE, removeNumbers=TRUE, ngrams=1)
dfm1.news.64 <- dfm(tok1.news.64) 
tok3.news.64 <- tokenize(v.news.64, removePunct=TRUE, removeNumbers=TRUE, ngrams=3)
dfm3.news.64 <- dfm(tok3.news.64) 
tok1.twitter.64 <- tokenize(v.twitter.64, removePunct=TRUE, removeNumbers=TRUE, ngrams=1)
dfm1.twitter.64 <- dfm(tok1.twitter.64) 
tok3.twitter.64 <- tokenize(v.twitter.64, removePunct=TRUE, removeNumbers=TRUE, ngrams=3)
dfm3.twitter.64 <- dfm(tok3.twitter.64) 
# tok1.blogs.8 <- tokenize(v.blogs.8, removePunct=TRUE, removeNumbers=TRUE, ngrams=1)
# dfm1.blogs.8 <- dfm(tok1.blogs.8) 
# tok3.blogs.8 <- tokenize(v.blogs.8, removePunct=TRUE, removeNumbers=TRUE, ngrams=3)
# dfm3.blogs.8 <- dfm(tok3.blogs.8) 
# tok1.blogs.1 <- tokenize(v.blogs.1, removePunct=TRUE, removeNumbers=TRUE, ngrams=1)
# dfm1.blogs.1 <- dfm(tok1.blogs.1) 
# tok3.blogs.1 <- tokenize(v.blogs.1, removePunct=TRUE, removeNumbers=TRUE, ngrams=3)
# dfm3.blogs.1 <- dfm(tok3.blogs.1) 


alln <- ls(all.names=TRUE)
names <- grep("[v|tok1|tok3|dfm1|dfm3].[news|blogs|twitter].*", alln, value=TRUE)
save(list=names, file="C:/Users/Phil/repos/ds-capstone/data/pre-proc.RData")
