## The option below is used upon initializing the rJava environment
## and may be placed in an .Renviron or .Rprofile file
options(java.parameters = c("-Djava.awt.headless=true", "-Xmx1g") );

#clear console
cat("\014") 

#clear workspace
rm(list = ls())

#clear graphs
graphics.off()

#Import required libraries
library(readxl)
library(xlsx)
library(ggplot2)

#import sources
setwd("/Users/maximiliandeichmann/Development/MasterThesis")
source("hhiFunctions.R")

#Set working directory to access source file
setwd("/Users/maximiliandeichmann/Documents/Education/TUM/Semester_4/MA/04_Statistics/Datensatz")

df <- read_excel("Original_Adapted.xlsx", col_types = c("numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "date", "numeric", "text"))

#initial data analysis
qplot(data = df, x = df$`Deal size`)
qplot(data = df, x = df$`Gross IRR`)
qplot(data=df, x = df$Investor_fund_ID)
qplot(data=df, x = df$`Primary Industry Group`)
qplot(data=df, x = df$`Primary Industry Code`)
qplot(data=df, x = df$`Primary Industry Sector`)
qplot(data=df, x = df$Stage)

#add HHL
df <- hhi(df,"Company country", "GeoHHI")
df <- hhi(df,"Stage", "StageHHI")
df <- hhi(df,"Primary Industry Group", "PIGHHI")
df <- hhi(df,"Primary Industry Code", "PICHHI")
df <- hhi(df,"Primary Industry Sector", "PISHHI")

qplot(data=df, x = df$GeoHHI)
qplot(data=df, x = df$StageHHI)
qplot(data=df, x = df$PIGHHI)
qplot(data=df, x = df$PICHHI)
qplot(data=df, x = df$PISHHI)

#save data
write.xlsx(df, file="Original_hhi.xlsx", sheetName="hhi")
setwd("/Users/maximiliandeichmann/Development/MasterThesis")
save(df,file="dataPreperation.Rda")
