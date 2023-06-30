# renv::install(".")

# renv::install("jasp-stats/jaspTools")
# jaspTools::setupJaspTools()

library(jaspTools)
setPkgOption("module.dirs", ".")
setPkgOption("reinstall.modules", FALSE)

options <- analysisOptions("Galileo")

data(galileo, package = "UsingR")
colnames(galileo) <- c("init_h", "h_d")

options$dependent <- "init_h"
options$covariate <- "h_d"

debugonce(jaspLearnScience:::Galileo)
result <- runAnalysis(options = options, dataset = galileo)
