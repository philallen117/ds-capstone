# profanity.R

if(!exists("profanities")) {
  profanities <- readLines("data/profanities.txt", encoding = "UTF-8", warn=FALSE, skipNul=TRUE)
}