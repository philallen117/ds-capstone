# sampling.R
# setwd("C:/Users/Phil/repos/ds-capstone")

sampFromConn <- function(inconn, outconn, lambda) {
  set.seed(lambda) # repeatable
  l <- ""
  while (length(l) > 0) {
    r <- rpois(1,lambda-1)
    readLines(inconn, r, ok=TRUE)
    l <- readLines(inconn, 1, ok=TRUE)
    if(length(l) > 0) writeLines(l, outconn)
  }
}

sampdata <- function(lambda, txtsource) {
  localname <- paste0("en_US.", txtsource, ".txt")
  inname <- paste0("data/Coursera-SwiftKey/final/en_US/", localname)
  inconn <- file(inname, "r")
  outname <- paste("data/samp", lambda, localname, sep = ".")
  outconn <- file(outname, "w")
  sampFromConn(inconn, outconn, lambda)
  close(outconn)
  close(inconn)
}

# sampdata(64, "blogs")
# sampdata(64, "twitter")
# sampdata(64, "news")
# sampdata(8, "blogs")
# sampdata(8, "twitter")
# sampdata(8, "news")