library(data.table)
library(dplyr)
library(stringi)
source("clean.R")
source("tokenize.R")
source("split.R")

v <-readLines("data/cat.txt")
t < tokenize(clean(v), 1, 5)
d1 <- AddWordAndContext(t)
d2 <- freqTable(t)

# returns table phrase, frequency, word, context
freqTable <- function(tokens) {
  d <- data.table(phrase=tokens, frequency=1)
  d <- group_by(d, phrase) %>%
    summarise(phrase, frequency=sum(frequency)) %>% 
    mutate(word=word(phrase), context=context(phrase))
  d
}

# Could I do this without table and dataframe .. data.table(phrase=tokenizedCorpus)
# Need some simple test data to compare outputs
AddWordAndContext <- function (tokenizedCorpus) {
  frequencyFrame <- data.frame(table(tokenizedCorpus))
  frequencyFrame$tokenizedCorpus  <- as.character(frequencyFrame$tokenizedCorpus)
  frequencyDataTable  <- data.table(frequencyFrame)    
  setnames(frequencyDataTable, c("phrase", "frequency"))
  setkey(frequencyDataTable, phrase)
  rm(frequencyFrame)
  frequencyDataTable[ , word := GetLastWordInPhrase(as.character(phrase)), by = phrase]
  frequencyDataTable[ , context := RemoveLastWord(as.character(phrase)), by = phrase]
  frequencyDataTable
}
# Leave these alone but get rid and replace with split
GetLastWordInPhrase  <- function(phrase){
  # Extracts the last word in a sequence of words, specified as a phrase.
  # Returns:
  #   A character vector of one word.
  stopifnot (is.character (phrase))
  stopifnot (length (phrase) == 1)
  words <- SplitPhraseIntoWords(phrase)
  return(words [length (words)])
}
SplitPhraseIntoWords  <- function(phrase){
  stopifnot (is.character (phrase))
  words <- unlist (strsplit (phrase, split = "[ ]+"))
  return(words [nchar (words) > 0])
}
RemoveLastWord  <- function(phrase){
  stopifnot (is.character (phrase))
  stopifnot (length (phrase) == 1)
  words <- SplitPhraseIntoWords (phrase)
  wordsWithoutLastWord <- words [1:length (words)-1]
  phraseWithoutLastWord  <- paste(wordsWithoutLastWord, collapse = " ")
  return(phraseWithoutLastWord)
}

# return table of phrase, frequency, word, context (KEY?), probability, gram, rank
# gram is n as in n-gram
# Try using dplyr chaining to avoid holding on to memory.
# Break out into build at LowerSpecificUnigramProbabilities !!!!!!
CreateTermProbabilityTableFromCorpus  <- function(tokenizedCorpus, reduceUniGramProbabilities, numberOfResultsByPhrase){ 
  # Add additional columns for the target word and context
  tokenizerFrequency  <- AddWordAndContext(tokenizedCorpus)
  # phrase, word, context, frequency
  contexts <- group_by(tokenizerFrequency, context, word, phrase) 
  tokenizerFrequency  <- summarise(contexts, contextCount=sum(frequency))
  setnames(tokenizerFrequency, c("context", "contextCount"))
  # Encoding(tokenizerFrequency$word)  <- c("unknown")
  # Encoding(tokenizerFrequency$phrase)  <- c("unknown")
  # Encoding(tokenizerFrequency$context)  <- c("unknown")
  Encoding(tokenizerFrequency$context)  <- c("unknown")
  setkey(tokenizerFrequency, context)
  tokenizerFrequency [tokenizerFrequency, probability := frequency/contextCount]
  # Add additional columns for the n-gram value
  tokenizerFrequency  <- AddNgramValue(tokenizerFrequency)
  
  if (reduceUniGramProbabilities){
  tokenizerFrequency  <- LowerSpecificUnigramProbabilities(tokenizerFrequency)
  }
  gc()
  setkey(tokenizerFrequency, context)
  tokenizerFrequency  <- AddRankForEachContext(tokenizerFrequency)
  tokenizerFrequencySortedRankedAndLimited  <- tokenizerFrequency[rank <= numberOfResultsByPhrase]
  return(tokenizerFrequencySortedRankedAndLimited)
}

# Making table incrementally ...
# Could store table when frequencies but before probs, and do probs after
# Or combine full table on basis of weighted average probs
# either way need to add data.tables
# dmodel <- readRDS("data/darragh.rds")
# smodel <- readRDS("data/m.100.train.sentences.rds")
# d <- dmodel$nGramModel
# s <- smodel$nGramModel

# # take training data a chunk at a time
# # initial n-gram data.table
# gramCounts <- data.table(gram=character(), n= integer(), count=integer, key="gram")
# CHUNK <- 100
# corp <- VCorpus(c("the cat sat on the mat", "on the mat sat the cat"))
# l <- length(corp)
# chunks <- l %/% CHUNK
# if (CHUNK * chunks < l) chunks <- chunks + 1
# low <- 1
# while(low <= l) {
#   # tokenize and count each segment
#   high <- min(low + CHUNK - 1, l)
#   gramCounts <- addToGramCounts(gramCounts, corp[low:high])
#   low <- low + CHUNK
# }
# addToGramCounts <- function(gramCounts, corp){
#   dtm <- DocumentTermMatrix(corp, control=list(tokenize = WordTokenizer)) 
#   
# }

# he also removed all single 3grams, which can be justified in terms of back-off
# relationship of back-off and smoothing ??? 
# pc.news <- PCorpus(vc.news.sc, dbControl = list(dbName = "news.raw.64.db", dbType = "DB1"))