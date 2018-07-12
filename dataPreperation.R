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
dealdf<- dealdf[dealdf$Gross_IRR < quantile(dealdf$Gross_IRR, probs = c(0.01, 0.99)),]

# create dummy vector
controlVector <- c()

#add HHI on deal level
hhis <- c("GeoHHI","StageHHI","PIGHHI","PICHHI","PISHHI")
fundhhis <- c("Fund_GeoHHI","Fund_StageHHI","Fund_PIGHHI","Fund_PICHHI","Fund_PISHHI", "Fund_AvgHHI")
dealdf<- hhi(dealdf,"Company_Country", "GeoHHI")
dealdf<- hhi(dealdf,"Company_Stage", "StageHHI")
dealdf<- hhi(dealdf,"Primary_Industry_Group", "PIGHHI")
dealdf<- hhi(dealdf,"Primary_Industry_Code", "PICHHI")
dealdf<- hhi(dealdf,"Primary_Industry_Sector", "PISHHI")

# create fund level data
funddf <- fundData(dealdf)

# drop top and bottom 5% quantile from irr deals
funddf <- funddf[funddf$Number_Investments > 2,] #quantile(funddf$Number_Investments, probs = c(0.01,1)),]

# add fund level hhi to deal levels
dealdf <- merge(dealdf,funddf[ , c(fundhhis, "Fund_ID", "Fund_SD")], by.x = "Investor_fund_ID", by.y = "Fund_ID")

# year dummy creation
new <- as.data.frame.matrix(table(sequence(nrow(dealdf)), substring(dealdf$Deal_Date,1,4)))
controlVector = c(controlVector,colnames(new))
print(controlVector)
dealdf<- cbind(dealdf, new)



# create grouped hhi
# groupdf <- hhiBuckets(10,funddf)


# time series
# timedf <- hhiTimeSeries(df, c("GeoHHI","StageHHI","PIGHHI","PICHHI","PISHHI"))

#save data
setwd("/Users/maximiliandeichmann/Development/MasterThesis")
save(controlVector, file = "controlVector.RData")
save(dealdf,file="dataPreperation_deal.Rda")
save(funddf,file="dataPreperation_fund.Rda")
# save(groupdf,file="dataPreperation_group.Rda")
setwd("/Users/maximiliandeichmann/Documents/Education/TUM-BWL/Semester_4/MA/04_Statistics/Datensatz")
excel_export(list(dealdf,funddf), "dataPreperation.xlsx", table_names=c("deal", "fund"))

