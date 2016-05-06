# clean.R
library(tm)
source("profanity.R")

hashtag  <- "#\\w+"
emailAddress  <- "(\\b\\S+\\@\\S+\\..{1,3}(\\s)?\\b)"
mentions  <- "@\\w+"
urls  <- "http[^[:space:]]*"
slashes  <- "/|@|\\|"
nonStandardCharacters  <- "[^A-Za-z0-9 ]+" # This does not seem to pick up the "è" character in a corpus, but does in a simple character vector.
specialChars  <- "[^\\p{L}\\s[']]+"

convertToEmpty <- content_transformer(function(x, pattern) gsub(pattern, "", x, fixed=TRUE))
convertToSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x, fixed=TRUE))
replaceSpecialChars <- content_transformer(function(x) stri_replace_all_regex(x, specialChars,""))

clean  <- function(corpus) { 
  corpus <- tm_map(corpus, removeWords, profanities, lazy = FALSE)
  corpus <- tm_map(corpus, content_transformer(tolower), lazy = FALSE)
  corpus <- tm_map(corpus, removeNumbers, lazy = FALSE)
  corpus <- tm_map(corpus, removePunctuation, lazy = FALSE) 
  corpus <- tm_map(corpus, convertToEmpty, specialChars, lazy = FALSE) # Issue here in picking up "è" characters, for example.    corpus <- tm_map(corpus, convertToEmpty, hashtag, lazy = FALSE)
  corpus <- tm_map(corpus, convertToEmpty, emailAddress, lazy = FALSE)
  corpus <- tm_map(corpus, convertToEmpty, mentions, lazy = FALSE)
  corpus <- tm_map(corpus, convertToEmpty, urls, lazy = FALSE)
  corpus <- tm_map(corpus, convertToSpace, slashes, lazy = FALSE)
  corpus <- tm_map(corpus, convertToEmpty, nonStandardCharacters, lazy = FALSE)
  corpus <- tm_map(corpus, stripWhitespace, lazy = FALSE)
  corpus <- tm_map(corpus, PlainTextDocument, lazy = FALSE)
  corpus
}
