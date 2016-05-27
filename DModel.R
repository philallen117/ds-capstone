library("R.utils")
library("tm")
library("SnowballC")
library("stringi")
library("plyr")
library("dplyr")
library("tidyr")
library("data.table")

source("utils/Constant.R")
source("utils/Init.R")
source("utils/Corpus.R")

# We must set the heap size before loading RWeka and other Java dependent libraries.
setJavaHeapSizeGB(10)
library("RWeka")
library("openNLP")

GetSourceFileNames  <- function() {
    # Returns the names of the uncompressed source files used to build the model.
    # The UnCompressFiles function below can be used to create these files from the zipped originals.
    fileNames <- c(
        "data/en_US.blogs.txt",
        "data/en_US.news.txt",
        "data/en_US.twitter.txt")
    return(fileNames)
}

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

AddRankForEachContext  <- function(termFrequencyTable){
    tokenizerFrequencyDataTableSorted  <- termFrequencyTable[order(context, -probability)]
    tokenizerFrequencyDataTableSortedAndRanked  <- tokenizerFrequencyDataTableSorted[, rank :=1:.N, by = context]
    return(tokenizerFrequencyDataTableSortedAndRanked)
}

AddNgramValue <- function (termFrequencyTable) {
    regex <- paste0 ("[", wordDelimiter, "]+")
    
    # For each of the entries, add an additional column to indicate the n-gram, by counting the number of words.
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

SetEncodingOnColumns  <- function(dataTable, columns, encodingType){
    lapply(columns, function(x) Encoding(dataTable[, x])  <- encodingType)
    return(dataTable)
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

SaveModel  <- function(destinationDirectory = "model", modelObject, modelName){
    saveRDS(ngramModelObject, file.path(destinationDirectory, modelName))
}

RetrieveModel  <- function(sourceDirectory = "model", modelName){
    return(readRDS(file.path(sourceDirectory, modelName)))
}

BuildModel  <- function(samplePercentage = 0.00005, lowGram = 1, highGram = 6, 
                        maxTermsReturned = 8, directoryCorpus = "corpus", 
                        reduceUniGramProbabilities = TRUE, 
                        profanityFileBannedWordsUri = "http://www.bannedwordlist.com/lists/swearWords.txt"){
  
    # Create a random sample of the source files.
    CreateRandomSampleFromFiles(GetSourceFileNames(), samplePercentage, directoryCorpus)
    
    # Create a corpus, perform standard pre-processing and tokenize. Chained together to make efficient use of memory.
    tokenizedCorpus  <- TokenizeCorpus(CleanCorpus(CreateCorpus(directoryCorpus), profanityFileBannedWordsUri), lowGram:highGram)
    
    # Create an n-gram probability table from the tokenized corpus.
    termProbabilityTable  <- CreateTermProbabilityTableFromCorpus(tokenizedCorpus, reduceUniGramProbabilities, maxTermsReturned)
    
    # Return an object which constitues the n-gram model and parameter(s)
    ngramModelObject  <- list(nGramModel = termProbabilityTable, minN = lowGram, maxN = highGram, maxReturnVal = maxTermsReturned)
    return(ngramModelObject)
}