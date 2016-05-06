# assumes single spacing and at least a unigram
last <- function(gram) { 
  words <- unlist (strsplit (gram, split = " "))
  l <- length(words)
  words[l]
}

rest <- function(gram) { 
  words <- unlist (strsplit (gram, split = " "))
  l <- length(words)
  if(l==1) "" else paste(words[1:(l-1)], collapse=" ")
}
