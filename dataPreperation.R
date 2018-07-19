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


# stage data adoption
# "Early Stage VC","Later Stage VC","PE Growth/Expansion","PIPE","Buyout/LBO","Seed Round","Mezzanine","Convertible Debt","Debt - General","Spin-Off","Joint Venture","Secondary Transaction - Private","Platform Creation"
# PIPE: https://www.investopedia.com/terms/p/pipe.asp
# Platform creation: https://en.wikipedia.org/wiki/Platform_company
# convertible bond: https://www.investopedia.com/terms/c/convertiblebond.asp
# mezzanine: https://de.wikipedia.org/wiki/Mezzanine-Kapital
# secondary transaction: ?
# spinn off
uniqueStages <- unique(dealdf$Company_Stage)
dealdf$Company_Stage[dealdf$Company_Stage == "Buyout/LBO" ] <- "Later Stage VC"
dealdf$Company_Stage[dealdf$Company_Stage == "PE Growth/Expansion" ] <- "Later Stage VC"
dealdf$Company_Stage[dealdf$Company_Stage == "Platform Creation" ] <- "Later Stage VC"
dealdf$Company_Stage[dealdf$Company_Stage == "Convertible Debt" ] <- "Later Stage VC"
dealdf$Company_Stage[dealdf$Company_Stage == "Debt - General" ] <- "Later Stage VC"
dealdf$Company_Stage[dealdf$Company_Stage == "Joint Venture" ] <- "Later Stage VC"
dealdf$Company_Stage[dealdf$Company_Stage == "Mezzanine" ] <- "Later Stage VC"
dealdf$Company_Stage[dealdf$Company_Stage == "Secondary Transaction - Private" ] <- "Later Stage VC"
dealdf$Company_Stage[dealdf$Company_Stage == "PIPE" ] <- "Later Stage VC"
dealdf$Company_Stage[dealdf$Company_Stage == "Seed Round" ] <- "Early Stage VC"
dealdf$Company_Stage[dealdf$Company_Stage == "Spin-Off" ] <- "Early Stage VC"
dealdf$Company_Stage[dealdf$Company_Stage == "Later Stage VC" ] <- "Late Stage"
dealdf$Company_Stage[dealdf$Company_Stage == "Early Stage VC" ] <- "Early Stage"

# quantile filtering
# dealdf<- dealdf[dealdf$Gross_IRR < quantile(dealdf$Gross_IRR, probs = c(0.1,0.9)) & dealdf$Deal_Size < quantile(dealdf$Deal_Size, probs = c(0.1, 0.9)),]
# dealdf<- dealdf[dealdf$Gross_IRR < quantile(dealdf$Gross_IRR, probs = c(0.05, 0.95)),]
# dealdf<- dealdf[dealdf$Deal_Size < quantile(dealdf$Deal_Size, probs = c(0.05, 0.95)),]

# create dummy vector
controlVector <- c()

#add HHI on deal level
hhis <- c("GeoHHI","StageHHI","PIGHHI","PICHHI","PISHHI")
fundhhis <- c("Fund_GeoHHI","Fund_StageHHI","Fund_PIGHHI","Fund_PICHHI","Fund_PISHHI", "Fund_AvgHHI","LFund_GeoHHI","LFund_StageHHI","LFund_PIGHHI","LFund_PICHHI","LFund_PISHHI", "LFund_AvgHHI")
# sdfundhhis <- c("SD_Fund_GeoHHI","SD_Fund_StageHHI","SD_Fund_PIGHHI","SD_Fund_PICHHI","SD_Fund_PISHHI", "SD_Fund_Avg_GeoHHI","SD_Fund_Avg_StageHHI","SD_Fund_Avg_PIGHHI","SD_Fund_Avg_PICHHI","SD_Fund_Avg_PISHHI","SD_Fund_AvgHHI")
dealdf<- hhi(dealdf,"Company_Country", "GeoHHI")
dealdf<- hhi(dealdf,"Company_Stage", "StageHHI")
dealdf<- hhi(dealdf,"Primary_Industry_Group", "PIGHHI")
dealdf<- hhi(dealdf,"Primary_Industry_Code", "PICHHI")
dealdf<- hhi(dealdf,"Primary_Industry_Sector", "PISHHI")

# total return
dealdf$Total_Return <- dealdf$Gross_IRR * dealdf$Deal_Size

# year
dealdf$Deal_Year <- year(dealdf$Deal_Date)

# add log variables
dealdf$LGeoHHI = sqrt(max(dealdf$GeoHHI) - dealdf$GeoHHI)
dealdf$LStageHHI = sqrt(max(dealdf$StageHHI) - dealdf$StageHHI)
dealdf$LPIGHHI = sqrt(max(dealdf$PIGHHI) - dealdf$PIGHHI)
dealdf$LPICHHI = sqrt(max(dealdf$PICHHI) - dealdf$PICHHI)
dealdf$LPISHHI = sqrt(max(dealdf$PISHHI) - dealdf$PISHHI)
dealdf$LDeal_Size <- log(dealdf$Deal_Size)
dealdf$LGross_IRR <- log(dealdf$Gross_IRR+1)
dealdf$LTotal_Return <- log(dealdf$Total_Return)

# add sd differences from mean
#dealdf <- sdDistance(c(hhis,"Gross_IRR"),dealdf)

# average hhi
#dealdf$AvgHHI <- apply(dealdf[,12:16], 1, mean)
#dealdf$AvgHHI <- rowMeans(dealdf[c('GeoHHI', 'StageHHI', 'PIGHHI')])


# create fund level data
funddf <- fundData(dealdf)
#funddf <- sdDistance(c(fundhhis,"Fund_SD"),funddf)

# add fund level hhi to deal levels
dealdf <- merge(dealdf,funddf[ , c("Fund_ID","Operating_Years", "LOperating_Years", "Fund_SD", "Number_Investments", "LNumber_Investments", "Total_Investments", "LTotal_Investments",fundhhis)], by.x = "Investor_fund_ID", by.y = "Fund_ID")

# year dummy creation
new <- as.data.frame.matrix(table(sequence(nrow(dealdf)), substring(dealdf$Deal_Date,1,4)))
newNames <- paste("Y", colnames(new), sep="")
colnames(new) <- newNames
controlVector = c(controlVector,newNames)
dealdf<- cbind(dealdf, new)

# america dummy
dummy <- as.numeric(dealdf$Company_Country == "United States")
dealdf$UnitedStates = dummy


# create grouped hhi based on crossproduct
# groupdf <- hhiBuckets(10,funddf,c("Fund_GeoHHI","Fund_StageHHI","Fund_PIGHHI","Fund_PICHHI","Fund_PISHHI"))
groupdf <- hhiBuckets(10,dealdf,c("GeoHHI","StageHHI","PIGHHI","PICHHI","PISHHI"),c("Gross_IRR", "Deal_Size"))

# time series
# timedf <- hhiTimeSeries(df, c("GeoHHI","StageHHI","PIGHHI","PICHHI","PISHHI"))

# filter for at least 6 years of firm experience
dealdf <- dealdf[dealdf$Operating_Years >= 6 | dealdf$Number_Investments >= 5,]
funddf <- funddf[funddf$Operating_Years >= 6 | funddf$Number_Investments >= 5,]

#save data
setwd("/Users/maximiliandeichmann/Development/MasterThesis")
save(controlVector, file = "controlVector.RData")
save(dealdf,file="dataPreperation_deal.Rda")
save(funddf,file="dataPreperation_fund.Rda")
save(groupdf,file="dataPreperation_group.Rda")
setwd("/Users/maximiliandeichmann/Documents/Education/TUM-BWL/Semester_4/MA/04_Statistics/Datensatz")
excel_export(list(dealdf,funddf,groupdf), "dataPreperation.xlsx", table_names=c("deal", "fund", "group"))