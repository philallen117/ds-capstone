# notes.R
# Things to come back to
# setwd("C:/Users/Phil/repos/ds-capstone")

# UTF-8 This reads cleanly bit leaves quotes.
# Way to eliminate utf-8 quotes. Bit of a hack.
# My problem is UTF-8 quote symbols - have to find a way to put them into regexp
# And to match leading and trailing whitespace before after "scare quotes" and 'scare quotes'.
# Once gsub working, could take it into tm_map as follows.
# f <- content_transformer(function(x, pattern) gsub(pattern, "", x))
# code <- tm_map(code, f, "[!\"#$%&'*+,./)(:;<=>?@\][\\^`{|}~]")
# or do it like ...
# crude <- tm_map(crude, content_transformer(tolower))

library(tm)
library(RWeka)
library(slam)
source("pre-proc.R")
v.blogs.64 <- pre.proc("blogs", 64)
vc.blogs.64 <- VCorpus(VectorSource(v.blogs.64))
vc.blogs.64 <- tm_map(vc.blogs.64, removePunctuation)
vc.blogs.64 <- tm_map(vc.blogs.64, removeNumbers)
vc.blogs.64 <- tm_map(vc.blogs.64, stripWhitespace)
# vc64.blogs[[1]]$content

# Sets the default number of threads to use
options(mc.cores=2)
dtm.blogs.64 <- DocumentTermMatrix(vc.blogs.64, control=list(tokenize = WordTokenizer)) 
# WordTokenizer is from Weka. Is that available in Shiny? How about quanteda? 
# Also NGramTokenizer, but need to check WOW for control - default is n=3
freqTerms <- findFreqTerms(dtm.blogs.64, lowfreq = 100)

# The following does not work, so I cannot get a word cloud
sm <- as.simple_triplet_matrix(dtm.blogs.64)
freqs <- rowSums(sm)


# data("crude")
# crude <- as.VCorpus(crude)
# crude <- tm_map(crude, stripWhitespace)
# crude <- tm_map(crude, removePunctuation)
# crude <- tm_map(crude, removeNumbers)
# crude <- tm_map(crude, removeWords, stopwords("english"))
# crude <- tm_map(crude, stemDocument)# How to get stuff back out of a Corpus
# c2 <- data.frame(text=unlist(sapply(crude, `[`, "content")), stringsAsFactors=F)


# library(ngram)
# library(languageR)
# library(zipfR)
# library(markovchain)


