# scratch.R
# setwd("C:/Users/Phil/repos/ds-capstone")

library(quanteda)
# plot gives a word cloud
plot(dfm1.blogs.64, max.words=100)
plot(dfm1.blogs.64, min.freq=500)

wordCounts <- colSums(dfm1.blogs.64)
wordCountsSorted <- sort(wordCounts, decreasing = TRUE)
hist(log2(wordCounts))

docLensWds <- rowSums(dfm1.blogs.64)
docLensWdsSorted <- sort(docLensWds, decreasing = TRUE)
hist(docLensWds)
hist(log2(docLensWds))
# could also try with stop words

gram3Counts <- colSums(dfm3.blogs.64)
gram3CountsSorted <- sort(wordCounts, decreasing = TRUE)
hist(gram3Counts)
hist(log2(gram3Counts))
plot(dfm3.blogs.64.tok3, max.words=100)
# don't need stop words for this

topfeatures(dfm1.twitter.64)
topfeatures(dfm1.news.64)
topfeatures(dfm1.blogs.64)
topfeatures(dfm3.twitter.64)
topfeatures(dfm3.news.64)
topfeatures(dfm3.blogs.64)



library(ngram)


# library(languageR)
# library(zipfR)
# library(markovchain)
