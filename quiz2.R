source("predict.R")

inputs <- list(
"The guy in front of me just bought a pound of bacon, a bouquet, and a case of",
"You're the reason why I smile everyday. Can you follow me please? It would mean the",
"Hey sunshine, can you follow me and make me the",
"Very early observations on the Bills game: Offense still struggling but the",
"Go on a romantic date at the",
"Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my",
"Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some",
"After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little",
"Be grateful for the good times and keep the faith during the",
"If this isn't the cutest thing you've ever seen, then you must be")

guesses <- lapply(inputs, predictWord)
for (i in 1:length(guesses)) print(guesses[[i]])

myans <- c("beer", "world", "happiest", "defense", "cinema", "way", "time", "fingers", "bad", "insane")
# romantic date is not cinema