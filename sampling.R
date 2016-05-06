# sampling.R
# setwd!

sampFile <- function(path, fin){
  fullpath <- function(f) paste(path, f, sep="/")
  inlines <- readLines(fullpath(fin), encoding="UTF-8")
  l <- length(inlines)
  testSize <- floor(l/5)
  allIndexes <- seq(inlines)
  testIndexes <- sort(sample(allIndexes, testSize))
  holdIndexes <- sort(sample(allIndexes[-testIndexes], testSize))
  trainIndexes <- allIndexes[-c(testIndexes,holdIndexes)]
  writeLines(inlines[testIndexes], fullpath(paste0("test.", fin)))
  writeLines(inlines[holdIndexes], fullpath(paste0("hold.", fin)))
  writeLines(inlines[trainIndexes], fullpath(paste0("train.", fin)))
 }

# sampFile("data", "blogs.raw.txt")
# sampFile("data", "news.raw.txt")
# sampFile("data", "tweets.raw.txt")
