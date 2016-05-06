library(stringi)
library(data.table)
vanish <- function(x, pattern) stri_replace_all_regex(pattern, "", x)
spacify <- function(x, pattern) gsub(pattern, " ", x, fixed=TRUE)

if(!exists("pred.model")) pred.model <- readRDS("data/pred.model.rds")

tokCleanInput <- function(input, maxPreds) {
  input <- iconv(input, from="UTF-8", to="ASCII//TRANSLIT", "")
  input <- stri_replace_all(input," ", regex="[^a-zA-Z]")
  words <- unlist(strsplit(input, split=" "))
  words[nchar(words) > 0]
}

# TODO check that model outputs ordered by rank

predictWord<- function(input, maxPreds=8)
  predictRow(input, model=pred.model, maxPreds=maxPreds)$word

predictRow <- function(input, model=pred.model, maxPreds=8) {
  maxContextLen <- model$maxN
  probTable <- model$nGramModel
  words <- tokCleanInput(input)
  candidates <- data.table()
  for (contextLen in maxContextLen:1) {
    context <- tail(words, contextLen)
    contextString <- paste(context, collapse=" ")
    candidates  <- probTable[context == contextString]
    if (nrow(candidates) > 0) break
  }
  if (nrow(candidates) == 0) {
      candidates  <- probTable[gram == 1]
  } 
  head(candidates, maxPreds)
}
