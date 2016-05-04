# scratch.R
# setwd!
# definitions from chunks in explore.Rmd

library(tm)
library(RWeka)
library(slam)

# other guy used tau for tokenisation, mostly because it does counting of ngrams.
# that will be ok when have got the text as clean batch of sentences, via opennlp perhaps

# he also removed all single 3grams, which can be justified in terms of back-off
# relationship of back-off and smoothing ??? 
library(tau)


load("data/pp.64.v.RData")

vc.news <- VCorpus(VectorSource(v[["news"]]))
vc.news.sc <- VectorSource(v[["vc.news"]])
pc.news <- PCorpus(vc.news.sc, dbControl = list(dbName = "news.raw.64.db", dbType = "DB1"))
# want to add meta-data on way in. here is one way.
Transcript <- data.frame(Words = Transcript, Speaker = NA, stringsAsFactors = FALSE)
Transcript$Speaker[regexpr("LEHRER: ", Transcript$Words) != -1] <- 1
Transcript$Speaker[regexpr("OBAMA: ", Transcript$Words) != -1] <- 2
Transcript$Speaker[regexpr("ROMNEY: ", Transcript$Words) != -1] <- 3
table(Transcript$Speaker)
Transcript$Speaker <- na.locf(Transcript$Speaker)

# Remove moderator:
Transcript <- Transcript[Transcript$Speaker != 1, ]

myCorpus <- Corpus(DataframeSource(Transcript))


# sentence markers: diolch and nhadiau don't come up, strangely.
# so sed those in for .<spc> and ?<spc> and then removepunc?
# so then i would have a corpus of sentences.
# can i do that within tm? just use NLP english model? http://stackoverflow.com/questions/18712878/r-break-corpus-into-sentences

# then, really want to find a better way to sample from corpus and keep a 40% hold-out.
# unless i reverse the sampling stuff I did on the raw file.

vc.blogs.64 <- tm_map(vc.blogs.64, removePunctuation)
vc.blogs.64 <- tm_map(vc.blogs.64, removeNumbers)
vc.blogs.64 <- tm_map(vc.blogs.64, stripWhitespace)
# vc64.blogs[[1]]$content

# Sets the default number of threads to use
options(mc.cores=2)
dtm.blogs.64 <- DocumentTermMatrix(vc.blogs.64, control=list(tokenize = WordTokenizer)) 

# maybe work on crude first?

data("crude")
TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tdmc <- TermDocumentMatrix(crude, control = list(tokenize = TrigramTokenizer))
tdmc <- TermDocumentMatrix(crude, control = list(tokenize = NGramTokenizer, 3))
inspect(tdmc[340:345,1:10])
findFreqTerms(tdmc, lowfreq = 5)[1:10]

plot(tdm, terms = findFreqTerms(tdm, lowfreq = 2)[1:50], corThreshold = 0.5)

f <- system.file("texts", "rcv1_2330.xml", package="tm")
rcv1 <- readRCV1asPlain(elem = list(content = readLines(f)), language = "en", id = "id1")
meta(rcv1)

convert_text_to_sentences <- function(text, lang = "en") {
  # Function to compute sentence annotations using the Apache OpenNLP Maxent
  # sentence detector employing the default model for language 'en'.
  # or try the one from qdap
  sentence_token_annotator <- Maxent_Sent_Token_Annotator(language = lang)
  # Convert text to class String from package NLP
  text <- as.String(text)
  # Sentence boundaries in text
  sentence.boundaries <- annotate(text, sentence_token_annotator)
  # Extract sentences
  sentences <- text[sentence.boundaries]
  return(sentences)
}

reshape_corpus <- function(current.corpus, FUN, ...) {
  # Extract the text from each document in the corpus and put into a list
  # text <- lapply(current.corpus, Content)
  # replace Content with content_transformer
  # Basically convert the text
  docs <- lapply(current.corpus, FUN, ...)
  docs <- as.vector(unlist(docs))
  # Create a new corpus structure and return it
  new.corpus <- Corpus(VectorSource(docs))
  return(new.corpus)
}
# Am i better off with dataframe or vectorsource to save it to PCorpus?

inspect(reshape_corpus(crude, convert_text_to_sentences))
