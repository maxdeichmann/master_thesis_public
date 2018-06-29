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
setwd("/Users/maximiliandeichmann/Documents/Education/TUM/Semester_4/MA/04_Statistics/Datensatz")

dealdf <- read_excel("Original_Adapted.xlsx", col_types = c("numeric", "numeric", "numeric", "text", "text", "text", "text", "text", "date", "numeric", "text"))

#add HHL
dealdf <- hhi(dealdf,"Company_Country", "GeoHHI")
dealdf <- hhi(dealdf,"Company_Stage", "StageHHI")
dealdf <- hhi(dealdf,"Primary_Industry_Group", "PIGHHI")
dealdf <- hhi(dealdf,"Primary_Industry_Code", "PICHHI")
dealdf <- hhi(dealdf,"Primary_Industry_Sector", "PISHHI")


# handle missing data

# IRR
# replace IRR NA with median
dealdf$Gross_IRR[is.na(dealdf$Gross_IRR)] <- median(dealdf$Gross_IRR, na.rm=TRUE)

# replace Deal size NA with median
dealdf$Deal_Size[is.na(dealdf$Deal_Size)] <- median(dealdf$Deal_Size, na.rm=TRUE)

# create fund level data
funddf <- fundData(dealdf)

# time series
# timedf <- hhiTimeSeries(df, c("GeoHHI","StageHHI","PIGHHI","PICHHI","PISHHI"))

#save data
excel_export(list(dealdf,funddf), "dataPreperation.xlsx", table_names=c("deal", "fund"))
setwd("/Users/maximiliandeichmann/Development/MasterThesis")
save(dealdf,file="dataPreperation_deal.Rda")
save(funddf,file="dataPreperation_fund.Rda")
