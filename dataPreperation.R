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

mscidf <- read_excel("msci_1.xlsx", col_types = c("numeric", "numeric"))
colnames(mscidf)[1] <- "Year"
colnames(mscidf)[2] <- "Return"

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

# Missing deal sizes values -> fund median
# Missing IRR values -> WA deal size
# Missing countries -> most investes
dealdf <- dataCleaning(dealdf)


# dealdf$Gross_IRR[is.na(dealdf$Gross_IRR)] <- dealdf$Fund_IRR[is.na(dealdf$Gross_IRR)]
# dealdf$Deal_Size[is.na(dealdf$Deal_Size)] <- dealdf$Fund_Deal_Size[is.na(dealdf$Deal_Size)]
# dealdf$Company_Country[is.na(dealdf$Company_Country)] <- dealdf$Popular_Country[is.na(dealdf$Company_Country)]


# create dummy vector
controlVector <- c()

# add deal level diversification indices
measures <- c("hh","e")
variables <- c("Company_Country","Company_Stage","PIG","PIC","PIS")
hhiIndices <- c("GeoHHI","StageHHI","PIGHHI","PICHHI","PISHHI")
lhhiIndices <- c("LGeoHHI","LStageHHI","LPIGHHI","LPICHHI","LPISHHI")
eiIndices <- c("GeoEI","StageEI","PIGEI","PICEI","PISEI")
leiIndices <- c("LGeoEI","LStageEI","LPIGEI","LPICEI","LPISEI")
divIndices <- c(hhiIndices,eiIndices)

fundhhiIndices <- c("Fund_GeoHHI","Fund_StageHHI","Fund_PIGHHI","Fund_PICHHI","Fund_PISHHI")
lfundhhiIndices <- c("LFund_GeoHHI","LFund_StageHHI","LFund_PIGHHI","LFund_PICHHI","LFund_PISHHI")
fundeiIndices <- c("Fund_GeoEI","Fund_StageEI","Fund_PIGEI","Fund_PICEI","Fund_PISEI")
lfundeiIndices <- c("LFund_GeoEI","LFund_StageEI","LFund_PIGEI","LFund_PICEI","LFund_PISEI")


fundDivIndices <- c()
for (a in divIndices) {
  fundDivIndices <- c(fundDivIndices,paste('Fund_', a, sep = ''))
}
for (a in divIndices) {
  fundDivIndices <- c(fundDivIndices,paste('LFund_', a, sep = ''))
}

for (a in measures) {
  for(i in 1:length(variables)) {
    if(a == "hh") {
      dealdf<- divers(dealdf,variables[i], hhiIndices[i],a)
    } else {
      dealdf<- divers(dealdf,variables[i], eiIndices[i],a) 
    }
  }
}


# deal year
dealdf$Deal_Year <- year(dealdf$Deal_Date)

# success variable
dealdf$Success[dealdf$Gross_IRR > median(dealdf$Gross_IRR) ] <- 1
dealdf$Success[dealdf$Gross_IRR <= median(dealdf$Gross_IRR) ] <- 0
dealdf$Loss[dealdf$Gross_IRR < 0 ] <- 1
dealdf$Loss[dealdf$Gross_IRR >= 0 ] <- 0
dealdf$TotalLoss[dealdf$Gross_IRR == -1 ] <- 1
dealdf$TotalLoss[dealdf$Gross_IRR > -1 ] <- 0

# countries: "United States",United Kingdom","Germany",
# "Spain","Denmark","France","Finland","China","Canada","Israel","Netherlands"
# "Switzerland","Sweden","India","Japan","South Korea","Czech Republic","Ireland","Hungary"

emerging <- c("Hungary","Czech Republic","South Korea","China","India")
developed <- c("Ireland","Japan","Switzerland","Sweden","Israel","Netherlands","Canada","United States","United Kingdom","Germany", "Spain","Denmark","France","Finland")
dealdf$developed <- print(dealdf$Company_Country == developed)


# data transformation
# independent
for (a in c(hhiIndices, eiIndices)) {
  newName <- paste('L', a, sep = '')
  divIndices <- c(divIndices,newName)
  dealdf[[newName]] <- log(dealdf[[a]]+1)
}
dealdf$LDeal_Size <- log(dealdf$Deal_Size+1)

# dependent
dealdf$LGross_IRR <- log(dealdf$Gross_IRR+1)

# create fund level data
funddf <- fundData(dealdf,divIndices,fundDivIndices, mscidf)

# add fund level hhi to deal levels
dealdf <- merge(dealdf,funddf[ , c("Fund_ID","Fund_IRR","Fund_Deal_Size","Operating_Years", "LOperating_Years", 
                                   "Fund_SD","LFund_SD", "Number_Investments", "LNumber_Investments", 
                                   "Total_Investments", "LTotal_Investments","Popular_Country","MSCI", 
                                   fundDivIndices)], 
                by.x = "Fund_ID", by.y = "Fund_ID")

# filter for at least 6 years of firm experience
dealdf <- dealdf[dealdf$Operating_Years >= 6 | dealdf$Number_Investments >= 5,]
funddf <- funddf[funddf$Operating_Years >= 6 | funddf$Number_Investments >= 5,]
# dealdf <- dealdf[dealdf$Gross_IRR != -1,]
# dealdf <- dealdf[dealdf$Fund_ID != 54,]

# create grouped hhi based on crossproduct
# groupdf <- hhiBuckets(10,dealdf,c("GeoHHI","StageHHI","PIGHHI","PICHHI","PISHHI"),c("Gross_IRR", "Deal_Size"))

#save data
setwd("/Users/maximiliandeichmann/Development/MasterThesis")

save(divIndices, file = "divIndices.RData")
save(fundDivIndices, file = "fundDivIndices.RData")
save(eiIndices, file = "eiIndices.RData")
save(hhiIndices, file = "hhiIndices.RData")


save(fundhhiIndices, file = "fundhhiIndices.RData")
save(lfundhhiIndices, file = "lfundhhiIndices.RData")

save(fundeiIndices, file = "fundeiIndices.RData")
save(lfundeiIndices, file = "lfundeiIndices.RData")

save(fundeiIndices, file = "fundrIndices.RData")
save(lfundeiIndices, file = "lfundrIndices.RData")


save(dealdf,file="dataPreperation_deal.Rda")
save(funddf,file="dataPreperation_fund.Rda")
setwd("/Users/maximiliandeichmann/Documents/Education/TUM-BWL/Semester_4/MA/04_Statistics/Datensatz")
excel_export(list(dealdf,funddf), "dataPreperation.xlsx", table_names=c("deal", "fund"))