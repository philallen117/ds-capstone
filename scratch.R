library(data.table)
library(dplyr)
library(stringi)

# Making table incrementally ...
# Could store table when frequencies but before probs, and do probs after
# Or combine full table on basis of weighted average probs
# either way need to add data.tables
# dmodel <- readRDS("data/darragh.rds")
# smodel <- readRDS("data/m.100.train.sentences.rds")
# d <- dmodel$nGramModel
# s <- smodel$nGramModel

# Could I do this without table and dataframe .. data.table(phrase=tokenizedCorpus)
# Need some simple test data to compare outputs
# returns table phrase, frequency, word, context
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

# Try using dplyr chaining to avoid holding on to memory.
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
  # Lower the probabilities of the unigrams for those occurring multiple times in the same bigram.
  tokenizerFrequency  <- LowerSpecificUnigramProbabilities(tokenizerFrequency)
}
# Cleanup memory.
gc()
setkey(tokenizerFrequency, context)
tokenizerFrequency  <- AddRankForEachContext(tokenizerFrequency)
tokenizerFrequencySortedRankedAndLimited  <- tokenizerFrequency[rank <= numberOfResultsByPhrase]
return(tokenizerFrequencySortedRankedAndLimited)
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

# he also removed all single 3grams, which can be justified in terms of back-off
# relationship of back-off and smoothing ??? 
# pc.news <- PCorpus(vc.news.sc, dbControl = list(dbName = "news.raw.64.db", dbType = "DB1"))