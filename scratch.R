# scratch.R

setwd("C:/Users/Phil/repos/ds-capstone")

# Generalise to source: en_US.[blogs|news|twitter].txt
# Get the input file path right. \data\Coursera-SwiftKey\final\en_US
# Try running for lambda 63, 7 ... but will take ages because read complete file

sampdata <- function(lambda) {
  # lambda <- 9 # 1 in 10
  full <- file("data/en_US.twitter.txt", "r")
  sampname <- paste0("data/samp", lambda + 1, "at", Sys.Date())
  samp <- file(sampname, "w")
  l <- readLines(full, 1, ok=TRUE)
  writeLines(l, samp)
  while (length(l) > 0) {
    r <- rpois(1,lambda)
    readLines(full, r, ok=TRUE)
  }
  close(full)
  close(samp)
}
