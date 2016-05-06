# tokenize.R
source("util.R")
setJavaHeapSizeGB(8)
library(RWeka)

# takes corpus returns vector
tokenize <- function(corpus, nlow, nhi) {
  tok <- function(x) NGramTokenizer(x, Weka_control(min=nlow, max=nhi))
  v  <- character()
  for (i in 1:length(corpus)) v <- c(v, tok(corpus[[i]]))
  v
}
