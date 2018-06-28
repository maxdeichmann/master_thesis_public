## The option below is used upon initializing the rJava environment
## and may be placed in an .Renviron or .Rprofile file
options(java.parameters = c("-Djava.awt.headless=true", "-Xmx1g") );

#clear console
cat("\014") 

#clear workspace
rm(list = ls())

#clear graphs
graphics.off()

load("dataPreperation.Rda")