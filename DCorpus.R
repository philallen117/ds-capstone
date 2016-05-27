library("R.utils")
library("tm")
library("RWeka")
library("openNLP")
library("SnowballC")
library("stringi")
library("Rgraphviz")
library("plyr")
library("dplyr")
library("tidyr")
library("data.table")

source("utils/Constant.R")
source("utils/File.R")

CreateCorpus <- function(sourceDirectory) {
    # Create a corpus from the three randomly-sampled files that have been created.
    # Returns:
    #   A corpus of documents.
    return(VCorpus(DirSource(file.path(getwd(), sourceDirectory)), readerControl = list(reader = readPlain, language="en_US", load=TRUE)))  
}

CleanCorpus  <- function(inputCorpus, profanityFileBannedWordsUri) {    
    profanityWords  <- GetProfanityWords(profanityFileBannedWordsUri)
    
    convertToEmpty <- content_transformer(function(x, pattern) gsub(pattern, "", x, fixed=TRUE))
    convertToSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x, fixed=TRUE))
    replaceSpecialChars <- content_transformer(function(x) stri_replace_all_regex(x, specialChars,""))
    
    originalInputCorpus  <- inputCorpus
    
    options(mc.cores=1) # This must be specified or core/processing errors are returned when executed on Mac.
    inputCorpus <- tm_map(inputCorpus, removeWords, profanityWords, lazy = FALSE)
    inputCorpus <- tm_map(inputCorpus, stripWhitespace, lazy = FALSE)
    inputCorpus <- tm_map(inputCorpus, content_transformer(tolower), lazy = FALSE)
    inputCorpus <- tm_map(inputCorpus, removeNumbers, lazy = FALSE)
    inputCorpus <- tm_map(inputCorpus, removePunctuation, lazy = FALSE) 
    inputCorpus <- tm_map(inputCorpus, convertToEmpty, specialChars, lazy = FALSE) # Issue here in picking up "è" characters, for example.    inputCorpus <- tm_map(inputCorpus, convertToEmpty, hashtag, lazy = FALSE)
    inputCorpus <- tm_map(inputCorpus, convertToEmpty, emailAddress, lazy = FALSE)
    inputCorpus <- tm_map(inputCorpus, convertToEmpty, mentions, lazy = FALSE)
    inputCorpus <- tm_map(inputCorpus, convertToEmpty, urls, lazy = FALSE)
    inputCorpus <- tm_map(inputCorpus, convertToSpace, slashes, lazy = FALSE)
    inputCorpus2 <- tm_map(inputCorpus, convertToEmpty, nonStandardCharacters, lazy = FALSE)
    inputCorpus <- tm_map(inputCorpus, PlainTextDocument, lazy = FALSE)
    
    return(inputCorpus)
}

TokenizeCorpus  <- function(inputCorpus, nGramRange) {
    combinedTokenizedCorpus  <- vector(mode="character")
    for (i in 1: length(inputCorpus) ){
        combinedTokenizedCorpus  <- c(combinedTokenizedCorpus, NGramTokenizer((inputCorpus[[i]]$content), Weka_control(min=min(nGramRange), max=max(nGramRange))))
    }
    return(combinedTokenizedCorpus)
}

SubsetCorpusForProcessing  <- function(corpusContent, nGramRange){
    subsettedData  <- vector(mode="character")
    segments  <- 1000
    fullCorpusContentLength  <- length(corpusContent)
    batch  <- floor(fullCorpusContentLength/segments)
    
    for(i in 1:segments){
        endPosition  <- i * batch
        startPosition  <- 1 + ((endPosition) - batch)
        subsettedData <- NGramTokenizer((corpusContent[startPosition:endPosition]), Weka_control(min=min(nGramRange), max=max(nGramRange)))
    }
    return(subsettedData)
}
