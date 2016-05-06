# sampling.R
# setwd!

sampFile <- function(path, fin){
  inlines <- readLines(file.path(path, fin), encoding="UTF-8")
  l <- length(inlines)
  testSize <- floor(l/5)
  allIndexes <- seq(1:l)
  testIndexes <- sort(sample(allIndexes, testSize))
  holdIndexes <- sort(sample(allIndexes[-testIndexes], testSize))
  trainIndexes <- allIndexes[-c(testIndexes,holdIndexes)]
  writeLines(inlines[testIndexes], file.path(path, paste0("test.", fin)))
  writeLines(inlines[holdIndexes], file.path(path, paste0("hold.", fin)))
  writeLines(inlines[trainIndexes], file.path(path, paste0("train.", fin)))
}

subSampFile <- function(path, fin, denom=1000) {
  inlines <- readLines(file.path(path, fin), encoding="UTF-8")
  l <- length(inlines)
  allIndexes <- seq(1:l)
  sampIndexes <- sort(sample(allIndexes, floor(l/denom)))
  outname <- file.path(path, paste0("s.", denom, ".", fin))
  writeLines(inlines[sampIndexes], outname)
}

# sampFile("data", "blogs.raw.txt")
# sampFile("data", "news.raw.txt")
# sampFile("data", "tweets.raw.txt")
# cat them together
# subSampTrain("data", "train.raw.txt")
