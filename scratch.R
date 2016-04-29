# scratch.R
# setwd!

library(parallel)
options(mc.cores = 4)
library(wordcloud)
library(quanteda)
library(sm)

load("data/pp.8.RData") # move to 1 in report

lenChars <- lapply(v.8, function(v){sort(nchar(v), decreasing=TRUE)})
lenWords <- lapply(dfm1, function(d){sort(rowSums(d), decreasing=TRUE)})
wordFreq <- lapply(dfm1, function(d){sort(colSums(d), decreasing=TRUE)})
gram2Freq <- lapply(dfm2, function(d){sort(colSums(d), decreasing=TRUE)})
gram3Freq <- lapply(dfm3, function(d){sort(colSums(d), decreasing=TRUE)})

flatCorps <- function(l) {
  twitter <- data.frame(length=l[["twitter"]], corpus="twitter")
  blogs <- data.frame(length=l[["blogs"]], corpus="blogs")
  news <- data.frame(length=l[["news"]], corpus="news")
  rbind(twitter, blogs, news)
}
lenCharsFlat <- flatCorps(lenChars)

# figures ...

# plot densities 
sm.density.compare(lenCharsFlat$length, lenCharsFlat$corpus, xlab="Length of document (characters)")
title(main="Density of document length (characters) for corpora")
# create value labels 
# add legend via mouse click
colfill<-c(2:(2+length(levels(cyl.f)))) 
legend(locator(1), levels(cyl.f), fill=colfill)


hist(lenWords[["twitter"]])
plot(density(lenWords[["twitter"]]))

# library(ngram)
# library(languageR)
# library(zipfR)
# library(markovchain)
