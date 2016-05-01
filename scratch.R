# scratch.R
# setwd!
# definitions from chunks in explore.Rmd

# load("data/pp.64.RData")
# load("data/pp.8.RData")
# load("data/pp.1.RData")

splitfile <- function(rate) {
  inname <- paste("data/pp", rate, "RData", sep=".")
  load(inname)
  names <- c("v", "dfm1", "dfm2", "dfm3")
  outname <- function(n) paste("data/pp", rate, n, "RData", sep=".")
  for(n in names) save(list=n, file=outname(n))
}

