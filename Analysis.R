#clear console
cat("\014") 

#clear workspace
rm(list = ls())

#Import required libraries
library(readxl)
library(diverse)

#Set working directory to access source file
setwd("/Users/maximiliandeichmann/Documents/Education/TUM/Semester_4/MA/04_Statistics/Datensatz")

df <- read_excel("Original_Adapted.xlsx")

#add HHL
maxFundNr = max(df$Investor_fund_ID)
funds <- 1:maxFundNr

for(a in seq(from=1, to=maxFundNr, by=1)) {
  print(a)
  subdf <- subset(df, Investor_fund_ID == a)
  out <- subdf[order(as.Date(subdf$`Deal date`)),]
  
  countries <- out$`Company country`
  uniqueCountries <- unique(countries)
  
  hhiMatrix <- matrix(0L, nrow = length(uniqueCountries), ncol = length(countries))
  
  for (i in seq(from=1,to=length(countries))) {
    index <- match(countries[i],uniqueCountries)
    if(i > 1) {
      for (x in seq(from=1, to=length(uniqueCountries))) {
        hhiMatrix[x,i] <- hhiMatrix[x,i-1]
      }  
    }
    
    hhiMatrix[index,i] <- hhiMatrix[index,i] + 1
  }

  hhi <- diversity(hhiMatrix, type='hh', category_row=TRUE)
  names(hhi) <- c("HHIGeo")
  
  if (a == 1) {
    output <- cbind(out,hhi)
  } else {
    output <- rbind(output,cbind(out,hhi))
  }
}




