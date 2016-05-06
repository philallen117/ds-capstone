# build-model.R
source("clean.R")
source("tokenize.R")
library(data.table)
library(dplyr)
library(stringi)
library(tm)
library(parallel)
options(mc.cores = 4)
source("util.R")
setJavaHeapSizeGB(8)
library(openNLP)

AddWordAndContext <- function (tokenizedCorpus) {
  frequencyFrame <- data.frame(table(tokenizedCorpus))
  frequencyFrame$tokenizedCorpus  <- as.character(frequencyFrame$tokenizedCorpus)
  frequencyDataTable  <- data.table(frequencyFrame)    
  setnames(frequencyDataTable, c("phrase", "frequency"))
  rm(frequencyFrame)
  frequencyDataTable[ , word := GetLastWordInPhrase(as.character(phrase)), by = phrase]
  frequencyDataTable[ , context := RemoveLastWord(as.character(phrase)), by = phrase]
  setkey(frequencyDataTable, phrase)
  return(frequencyDataTable)
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


AddRankForEachContext  <- function(termFrequencyTable){
  tokenizerFrequencyDataTableSorted  <- termFrequencyTable[order(context, -probability)]
  tokenizerFrequencyDataTableSortedAndRanked  <- tokenizerFrequencyDataTableSorted[, rank :=1:.N, by = context]
  return(tokenizerFrequencyDataTableSortedAndRanked)
}

AddNgramValue <- function (termFrequencyTable) {
  regex <- paste0 ("[", ' \r\n\t.,;:\\"()?!', "]+") # prob just single spaces here
  termFrequencyTable[, gram := unlist (lapply (stri_split (phrase, regex = regex), length)) ]
  return(termFrequencyTable)
}

LowerSpecificUnigramProbabilities  <- function(ngramFrequencyTable){
  bigramTokenizer  <- ngramFrequencyTable[gram==2,list(phrase, frequency, word, context, probability, gram)]
  unigramTokenizer  <- ngramFrequencyTable[gram==1,list(phrase, frequency, word, context, probability, gram)]
  
  countAllWordOccurrences  <- bigramTokenizer[, sum(frequency), by=word]
  setnames(countAllWordOccurrences, c("word", "allOccurrences"))
  
  for (thisWord in countAllWordOccurrences$word){
    numberOfBigrams  <- as.integer(count(bigramTokenizer[word==thisWord]))
    countAllWordOccurrences[word==thisWord, uniqueOccurrences := numberOfBigrams]
  }
  
  # Identify only those rows where a word is preceeded by the same context more than once.
  wordsWithRepeatedContexts  <- countAllWordOccurrences[allOccurrences!=uniqueOccurrences]
  
  setkey(unigramTokenizer, word, gram)
  setkey(wordsWithRepeatedContexts, word)
  
  # Lower the probability of each of the unigrams which are preceeded by the same context more than once.
  unigramTokenizer  <- unigramTokenizer[wordsWithRepeatedContexts, adjustedProbability := probability * (uniqueOccurrences / allOccurrences)]
  
  # Reduce the dataset to only those unigrams that require their probabilities adjusted.
  unigramTokenizer  <- unigramTokenizer[wordsWithRepeatedContexts]
  
  # Adjust the master list.
  ngramFrequencyTable[unigramTokenizer, probability := adjustedProbability]
  
  return(ngramFrequencyTable)
}

CreateTermProbabilityTableFromCorpus  <- function(tokenizedCorpus, reduce1gramProbs, numberOfResultsByPhrase){ 
  # Add additional columns for the target word and context
  tokenizerFrequency  <- AddWordAndContext(tokenizedCorpus)
  
  # Let us take an approach of dividing up our n-grams into the last word (i.e. the word to be predicted) and the
  # preceeding word(s) (i.e. the words setting the context).  Let us count the number of times the context occurs.
  contextTokenizerFrequency  <- tokenizerFrequency[, sum(frequency), by = context]
  setnames(contextTokenizerFrequency, c("context", "contextCount"))
  
  Encoding(tokenizerFrequency$word)  <- c("unknown")
  Encoding(tokenizerFrequency$phrase)  <- c("unknown")
  Encoding(tokenizerFrequency$context)  <- c("unknown")
  Encoding(contextTokenizerFrequency$context)  <- c("unknown")
  
  setkey(contextTokenizerFrequency, context)
  setkey(tokenizerFrequency, context)
  
  tokenizerFrequency [contextTokenizerFrequency, probability := frequency/contextCount]
  
  # Add additional columns for the n-gram value
  tokenizerFrequency  <- AddNgramValue(tokenizerFrequency)
  
  if (reduce1gramProbs){
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

build  <- function(source, nlo=1, nhi=5, maxTermsReturned=8, reduce1gramProbs=TRUE) {
  corpus <- VCorpus(source)
  corpus <- clean(corpus)
  corpus <- tokenize(corpus, nlo, nhi)
  termProbabilityTable  <- CreateTermProbabilityTableFromCorpus(corpus, reduce1gramProbs, maxTermsReturned)
  ngramModelObject  <- 
    list(nGramModel=termProbabilityTable, minN=nlo, maxN=nhi, maxReturnVal=maxTermsReturned)
  ngramModelObject
}

buildSaveFromFile <- function(infn, outfn="data/tmp.rds", nlo=1, nhi=5, maxTermsReturned=8) {
  v <- readLines(infn)
  m <- build(VectorSource(v), nlo, nhi, maxTermsReturned)
  rm(v)
  saveRDS(m, file=outfn)
}
