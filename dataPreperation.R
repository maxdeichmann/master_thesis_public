#clear console
cat("\014") 

#clear workspace
rm(list = ls())

## The option below is used upon initializing the rJava environment
## and may be placed in an .Renviron or .Rprofile file
options(java.parameters = c("-Djava.awt.headless=true", "-Xmx1g") );

#Import required libraries
library(readxl)
library(xlsx)

#import sources
setwd("/Users/maximiliandeichmann/Development/MasterThesis")
source("hhiFunctions.R")

#Set working directory to access source file
setwd("/Users/maximiliandeichmann/Documents/Education/TUM/Semester_4/MA/04_Statistics/Datensatz")

df <- read_excel("Original_Adapted.xlsx")

#add HHL
df <- hhi(df,"Company country", "GeoHHI")


write.xlsx(df, file="hhi.xlsx", sheetName="hhi")

#####check deal 33


