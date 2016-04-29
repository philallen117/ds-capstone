# pre-proc.R
# setwd!
# cp raw data to data/samp1.* so pre.proc works.
#
# pp(1) etc.

library(parallel)
options(mc.cores = 4)
library(quanteda)

vascii <- function (srce, rate) {
  fname <- paste("data/samp", rate, "en_US", srce, "txt", sep=".")
  v <- readLines(fname, encoding="UTF-8")
  v <- iconv(v, "UTF-8", "ASCII//TRANSLIT", "")
  v <- gsub("\\\"", "", v)
}

dfm.n <- function(v, n) {
  sapply(v, dfm, ngrams=n, removePunct=TRUE, removeNumbers=TRUE, removeTwitter=FALSE)
}

pp <- function(rate) {
  v <- list(blogs=vascii("blogs", rate),
            news=vascii("news", rate),
            twitter=vascii("twitter", rate))
  dfm1 <- dfm.n(v, 1)
  dfm2 <- dfm.n(v, 2)
  dfm3 <- dfm.n(v, 3)
  names <- c("v", "dfm1", "dfm2", "dfm3")
  oname <- paste("data/pp", rate, "RData", sep=".")
  save(list=names, file=oname)
}
