# build-model.R
source("clean.R")
source("tokenize.R")
source("split.R")
library(data.table)
library(dplyr)
library(stringi)
library(tm)
library(parallel)
options(mc.cores = 4)
source("util.R")
setJavaHeapSizeGB(8)
library(openNLP)
source("lower-probs.R")

simpleFrequencyTable <- function (tokenizedCorpus) {
  tt <- table(tokenizedCorpus)
  frequencyDataTable  <- data.table(tt)    
  setnames(frequencyDataTable, c("phrase", "frequency"))
  setkey(frequencyDataTable, "phrase")
  frequencyDataTable[ , word := word(phrase), by = phrase]
  frequencyDataTable[ , context := context(phrase), by = phrase]
  return(frequencyDataTable)
}

addProbsAndRanks  <- function(tokenizerFrequency, reduce1gramProbs, numberOfResultsByPhrase){ 
  # add probabilities of phrase given context
  contextTokenizerFrequency  <- tokenizerFrequency[, sum(frequency), by = context]
  setnames(contextTokenizerFrequency, c("context", "contextCount"))
  setkey(contextTokenizerFrequency, context)
  setkey(tokenizerFrequency, context)
  tokenizerFrequency[contextTokenizerFrequency, probability := frequency/contextCount]
  rm(contextTokenizerFrequency)
  # add which n
  tokenizerFrequency[, gram := stri_count(phrase, regex="\\S+")]
  if (reduce1gramProbs){
    adjustedUnigramProbs  <- getLoweredUnigramProbs(tokenizerFrequency)
    tokenizerFrequency[adjustedUnigramProbs, probability := adjustedProbability]
    rm(adjustedUnigramProbs)
  }
  setkey(tokenizerFrequency, context)
  # add rank
  # # no easy way to sort in place ... this creates a new dt ... remember to return it
  # tokenizerFrequency  <- tokenizerFrequency[order(context, -probability)]
  # tokenizerFrequency[, rank := 1:.N, by = context]
  return(tokenizerFrequency)
}

build  <- function(source, nlo=1, nhi=5, maxTermsReturned=8, reduce1gramProbs=TRUE) {
  corpus <- VCorpus(source)
  corpus <- clean(corpus)
  corpus <- tokenize(corpus, nlo, nhi)
  tokenizerFrequency <- simpleFrequencyTable(corpus)
  rm(corpus) # Get some memory back
  termProbabilityTable <- addProbs(tokenizerFrequency, reduce1gramProbs, maxTermsReturned)
  # no easy way to sort in place ... this creates a new dt
  tokenizerFrequency  <- tokenizerFrequency[order(context, -probability)]
  tokenizerFrequency[, rank := 1:.N, by = context]
  
  # limit results per context ... maybe use prob threshold instead?
  termProbabilityTable  <- termProbabilityTable[rank <= maxTermsReturned]
  ngramModelObject <- list(nGramModel=termProbabilityTable, minN=nlo, maxN=nhi, maxReturnVal=maxTermsReturned)
  ngramModelObject
}

buildSaveFromFile <- function(infn, outfn="data/tmp.rds", nlo=1, nhi=5, maxTermsReturned=8) {
  v <- readLines(infn)
  m <- build(VectorSource(v), nlo, nhi, maxTermsReturned)
  rm(v)
  saveRDS(m, file=outfn)
}
