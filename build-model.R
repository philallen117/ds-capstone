# build-model.R
source("clean.R")
source("tokenize.R")
source("split.R")

library(data.table)
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



CreateTermProbabilityTableFromCorpus  <- function(tokenizedCorpus, reduceUniGramProbabilities, numberOfResultsByPhrase){ 
  # Add additional columns for the target word and context
  tokenizerFrequency  <- AddWordAndContext(tokenizedCorpus)
  
  # Let us take an approach of dividing up our n-grams into the last word (i.e. the word to be predicted) and the
  # preceeding word(s) (i.e. the words setting the context).  Let us count the number of times the context occurs.
  contextTokenizerFrequency  <- tokenizerFrequency[, sum(frequency), by = context]
  setnames(contextTokenizerFrequency, c("context", "contextCount"))
  
  Encoding(tokenizerFrequency$word)  <- neutralEncoding
  Encoding(tokenizerFrequency$phrase)  <- neutralEncoding
  Encoding(tokenizerFrequency$context)  <- neutralEncoding
  Encoding(contextTokenizerFrequency$context)  <- neutralEncoding
  
  setkey(contextTokenizerFrequency, context)
  setkey(tokenizerFrequency, context)
  
  tokenizerFrequency [contextTokenizerFrequency, probability := frequency/contextCount]
  
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




corpus <- VCorpus(VectorSource(readLines("data/train.raw.txt")))
corpus <- cleanCorpus(corpus)
tokens  <- tokenize(corpus, 1, 4)

tt <- table(tokens)
freqTable  <- data.table(gram=names(tt), count=as.integer(tt))    
freqTable[ , word := last(as.character(gram)), by = gram]
freqTable[ , context := rest(as.character(gram)), by = gram]
setkey(freqTable, gram)

saveRDS(freqTable,file="data/train.freqTable.RData")

# Find frequencies

# Create an n-gram probability table from the tokenized corpus.

termProbabilityTable  <- CreateTermProbabilityTableFromCorpus(tokenizedCorpus, reduceUniGramProbabilities, maxTermsReturned)



# take training data a chunk at a time
# initial n-gram data.table
gramCounts <- data.table(gram=character(), n= integer(), count=integer, key="gram")

CHUNK <- 100
corp <- VCorpus(c("the cat sat on the mat", "on the mat sat the cat"))
l <- length(corp)
chunks <- l %/% CHUNK
if (CHUNK * chunks < l) chunks <- chunks + 1
low <- 1
while(low <= l) {
  # tokenize and count each segment
  high <- min(low + CHUNK - 1, l)
  gramCounts <- addToGramCounts(gramCounts, corp[low:high])
  low <- low + CHUNK
}

addToGramCounts <- function(gramCounts, corp){
  dtm <- DocumentTermMatrix(corp, control=list(tokenize = WordTokenizer)) 
  
}



