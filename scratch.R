# scratch.R
# setwd!
# definitions from chunks in explore.Rmd

load("data/pp.64.RData")
# load("data/pp.8.RData")
# load("data/pp.1.RData")

ggplot(feat3df, aes(x=normFreq, group=corpus, colour=corpus)) +
  geom_density() + 
  scale_x_continuous(trans=reverselog_trans(10)) +
  # scale_x_log10() +
  scale_colour_manual(values=cbbPalette) +
  xlab("Frequency of trigram") + 
  ggtitle("Density of trigram frequency by corpus")
