# set project home directory
library(rsconnect)
appFiles <- c("predict.R", "data/pred.model.rds", "server.R", "ui.R")
deployApp(appFiles=appFiles, launch.browser=TRUE)
