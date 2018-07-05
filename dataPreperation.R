# The option below is used upon initializing the rJava environment
# and may be placed in an .Renviron or .Rprofile file
options(java.parameters = c("-Djava.awt.headless=true", "-Xmx1g") );

# clear console
cat("\014") 

# clear workspace
rm(list = ls())

# clear graphs
graphics.off()

#Import required libraries
library(readxl)
library(xlsx)
library(ggplot2)
library(ImportExport)

# import sources
setwd("/Users/maximiliandeichmann/Development/MasterThesis")
source("helperFunctions.R")

# Set working directory to access source file
setwd("/Users/maximiliandeichmann/Documents/Education/TUM-BWL/Semester_4/MA/04_Statistics/Datensatz")

dealdf <- read_excel("Original_Adapted.xlsx", col_types = c("numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "date", "numeric", "text"))

# handle missing data
# replace IRR NA with median
dealdf$Gross_IRR[is.na(dealdf$Gross_IRR)] <- median(dealdf$Gross_IRR, na.rm=TRUE)

# replace Deal size NA with median
dealdf$Deal_Size[is.na(dealdf$Deal_Size)] <- median(dealdf$Deal_Size, na.rm=TRUE)


# drop top and bottom 5% quantile from irr deals
reduceddf <- dealdf[dealdf$Gross_IRR < quantile(dealdf$Gross_IRR, probs = c(0.1, 0.9)),]

#add HHL
reduceddf <- hhi(reduceddf,"Company_Country", "GeoHHI")
reduceddf <- hhi(reduceddf,"Company_Stage", "StageHHI")
reduceddf <- hhi(reduceddf,"Primary_Industry_Group", "PIGHHI")
reduceddf <- hhi(reduceddf,"Primary_Industry_Code", "PICHHI")
reduceddf <- hhi(reduceddf,"Primary_Industry_Sector", "PISHHI")

# year dummy creation
reduceddf <- cbind(reduceddf, as.data.frame.matrix(table(sequence(nrow(reduceddf)), substring(reduceddf$Deal_Date,1,4))))

# experience dummy

# create dummy vector
dummyVector <- c()
for (i in 1980:2012) {
  dummyVector <- c(dummyVector, as.character(i))
}

# create fund level data
funddf <- fundData(reduceddf)

# create grouped hhi
groupdf <- hhiBuckets(10,funddf)


# time series
# timedf <- hhiTimeSeries(df, c("GeoHHI","StageHHI","PIGHHI","PICHHI","PISHHI"))

#save data
excel_export(list(reduceddf,funddf,groupdf), "dataPreperation.xlsx", table_names=c("deal", "fund", "group"))
setwd("/Users/maximiliandeichmann/Development/MasterThesis")
save(reduceddf,file="dataPreperation_deal.Rda")
save(funddf,file="dataPreperation_fund.Rda")
save(groupdf,file="dataPreperation_group.Rda")