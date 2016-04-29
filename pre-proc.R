# pre-proc.R
# setwd("C:/Users/Phil/repos/ds-capstone")
# cp raw data to data/samp1.* so pre.proc works.

vascii <- function (srce, rate) {
  fname <- paste("data/samp", rate, "en_US", srce, "txt", sep=".")
  v <- readLines(fname, encoding="UTF-8")
  v <- iconv(v, "UTF-8", "ASCII//TRANSLIT", "")
  v <- gsub("\\\"", "", v)
}

v.64 <- list(blogs=vascii("blogs", 64), news=vascii("news", 64), twitter=vascii("twitter", 64))
v.8 <- list(blogs=vascii("blogs", 8), news=vascii("news", 8), twitter=vascii("twitter", 8))
# v.1 <- list(blogs=vascii("blogs", 1), news=vascii("news", 1), twitter=vascii("twitter", 1))


library(quanteda)

dfm.n.r <- function(n, r) {
  vr <- get(paste0("v.", r))
  sapply(vr,
         function(v) { dfm(v, ngrams=n,
                           removePunct=TRUE, removeNumbers=TRUE, removeTwitter=FALSE)})
}
dfm1.64 <- dfm.n.r(1, 64); dfm2.64 <- dfm.n.r(2, 64); dfm3.64 <- dfm.n.r(3, 64)
dfm1.8 <- dfm.n.r(1, 8); dfm2.8 <- dfm.n.r(2, 8); dfm3.8 <- dfm.n.r(3, 8)
# dfm1.1 <- dfm.n.r(1, 1); dfm2.1 <- dfm.n.r(2, 1); dfm3.1 <- dfm.n.r(3, 1)

names <- grep("(v|dfm11|dfm2|dfm3).(1|9|64)", ls(all.names=TRUE), value=TRUE)
save(list=names, file="C:/Users/Phil/repos/ds-capstone/data/pre-proc.RData")
