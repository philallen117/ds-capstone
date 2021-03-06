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
  gc()
  v <- list(blogs=vascii("blogs", rate),
            news=vascii("news", rate),
            twitter=vascii("twitter", rate))
  dfm1 <- dfm.n(v, 1)
  gc()
  dfm2 <- dfm.n(v, 2)
  gc()
  dfm3 <- dfm.n(v, 3)
  gc()
  names <- c("v", "dfm1", "dfm2", "dfm3")
  oname <- paste("data/pp", rate, "RData", sep=".")
  save(list=names, file=oname)
}

splitfile <- function(rate) {
  inname <- paste("data/pp", rate, "RData", sep=".")
  load(inname)
  names <- c("v", "dfm1", "dfm2", "dfm3")
  outname <- function(n) paste("data/pp", rate, n, "RData", sep=".")
  for(n in names) save(list=n, file=outname(n))
}

