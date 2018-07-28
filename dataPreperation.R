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
library(MASS)

# import sources
setwd("/Users/maximiliandeichmann/Development/MasterThesis")
source("helperFunctions.R")

# Set working directory to access source file
setwd("/Users/maximiliandeichmann/Documents/Education/TUM-BWL/Semester_4/MA/04_Statistics/Datensatz")

dealdf <- read_excel("Original_Adapted.xlsx", col_types = c("numeric", "numeric", "numeric", "text", "text", "text", 
                                                            "text", "text", "date", "numeric", "text"))
colnames(dealdf)[2] <- "Fund_ID"
colnames(dealdf)[6] <- "PIS"
colnames(dealdf)[4] <- "PIG"
colnames(dealdf)[5] <- "PIC"


# stage data adoption
# "Early Stage VC","Later Stage VC","PE Growth/Expansion","PIPE","Buyout/LBO","Seed Round","Mezzanine",
# "Convertible Debt","Debt - General","Spin-Off","Joint Venture","Secondary Transaction - Private","Platform Creation"
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

# create dummy vector
controlVector <- c()

# add HHI on deal level
hhis <- c("GeoHHI","StageHHI","PIGHHI","PICHHI","PISHHI")
fundhhis <- c("Fund_GeoHHI","Fund_StageHHI","Fund_PIGHHI","Fund_PICHHI","Fund_PISHHI", "Fund_AvgHHI","LFund_GeoHHI",
              "LFund_StageHHI","LFund_PIGHHI","LFund_PICHHI","LFund_PISHHI", "LFund_AvgHHI")
dealdf<- hhi(dealdf,"Company_Country", "GeoHHI")
dealdf<- hhi(dealdf,"Company_Stage", "StageHHI")
dealdf<- hhi(dealdf,"PIG", "PIGHHI")
dealdf<- hhi(dealdf,"PIC", "PICHHI")
dealdf<- hhi(dealdf,"PIS", "PISHHI")

# deal year
dealdf$Deal_Year <- year(dealdf$Deal_Date)

# data transformation
# independent
dealdf$LGeoHHI = log(dealdf$GeoHHI+1)
dealdf$LStageHHI = log(dealdf$StageHHI+1)
dealdf$LPIGHHI = log(dealdf$PIGHHI+1)
dealdf$LPICHHI = log(dealdf$PICHHI+1)
dealdf$LPISHHI = log(dealdf$PISHHI+1)
dealdf$LDeal_Size <- log(dealdf$Deal_Size)

# dependent
dealdf$LGross_IRR <- log(dealdf$Gross_IRR+1)

# create fund level data
funddf <- fundData(dealdf)

# add fund level hhi to deal levels
dealdf <- merge(dealdf,funddf[ , c("Fund_ID","Fund_IRR","Fund_Deal_Size","Operating_Years", "LOperating_Years", "Fund_SD", "Number_Investments", 
                                   "LNumber_Investments", "Total_Investments", "LTotal_Investments",fundhhis,"Mean_PIS","Mean_PIG","Mean_PIC","Popular_Country")], 
                by.x = "Fund_ID", by.y = "Fund_ID")

# filter for at least 6 years of firm experience
dealdf <- dealdf[dealdf$Operating_Years >= 6 | dealdf$Number_Investments >= 5,]
funddf <- funddf[funddf$Operating_Years >= 6 | funddf$Number_Investments >= 5,]

# Missing IRR values
dealdf$Gross_IRR[is.na(dealdf$Gross_IRR)] <- dealdf$Fund_IRR[is.na(dealdf$Gross_IRR)]

# Missing deal sizes
dealdf$Deal_Size[is.na(dealdf$Deal_Size)] <- dealdf$Fund_Deal_Size[is.na(dealdf$Deal_Size)]

# Missing countries
dealdf$Company_Country[is.na(dealdf$Company_Country)] <- dealdf$Popular_Country[is.na(dealdf$Company_Country)]

# create grouped hhi based on crossproduct
groupdf <- hhiBuckets(10,dealdf,c("GeoHHI","StageHHI","PIGHHI","PICHHI","PISHHI"),c("Gross_IRR", "Deal_Size"))

#save data
setwd("/Users/maximiliandeichmann/Development/MasterThesis")
save(controlVector, file = "controlVector.RData")
save(dealdf,file="dataPreperation_deal.Rda")
save(funddf,file="dataPreperation_fund.Rda")
save(groupdf,file="dataPreperation_group.Rda")
setwd("/Users/maximiliandeichmann/Documents/Education/TUM-BWL/Semester_4/MA/04_Statistics/Datensatz")
excel_export(list(dealdf,funddf,groupdf), "dataPreperation.xlsx", table_names=c("deal", "fund", "group"))