#clear console
cat("\014") 

#clear workspace
rm(list = ls())

#Import required libraries
library(readxl)
library(hhi)  
library(diverse)

#Set working directory to access source file
setwd("/Users/maximiliandeichmann/Documents/Education/TUM/Semester_4/MA/04_Statistics/Datensatz")

df <- read_excel("Original.xlsx")

#add HHL
maxFundNr = max(df$Investor_fund_ID)
funds <- 1:maxFundNr

subdf <- subset(df, Investor_fund_ID == 2)
out <- subdf[order(as.Date(subdf$`Deal date`)),]

out$HHIGeo = 1
diversity(data = out, type = "hh")