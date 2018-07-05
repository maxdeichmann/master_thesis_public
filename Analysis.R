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
library(plyr)
library(ggplot2)

load("dataPreperation_deal.Rda")
load("dataPreperation_fund.Rda")

# descriptives
summary(dealdf)
summary(funddf)





# initial data analysis
qplot(data = dealdf, x = dealdf$Deal_Size)
# qplot(data = df, x = df$Gross_IRR)
# qplot(data=df, x = df$Investor_fund_ID)
# qplot(data=df, x = df$Primary_Industry_Group)
# qplot(data=df, x = df$Primary_Industry_Sector)
# qplot(data=df, x = df$Primary_Industry_Code)
qplot(data=dealdf, x = dealdf$Company_Stage)

# plot HHIs
# qplot(data=df, x = df$GeoHHI)
# qplot(data=df, x = df$StageHHI)
# qplot(data=df, x = df$PIGHHI)
# qplot(data=df, x = df$PICHHI)
# qplot(data=df, x = df$PISHHI)

# uniqueGroup <- unique(df$Primary_Industry_Group)
# uniqueCode <- unique(df$Primary_Industry_Code)
# uniqueSector <- unique(df$Primary_Industry_Sector)

qplot(data = funddf, x = funddf$Fund_IRR)


# linear analysis
scatter.smooth(x=funddf$StageHHI, y=funddf$Fund_IRR, main="geo")
# scatter.smooth(x=dealdf$StageHHI, y=dealdf$Deal_Date, main="FIRR - STageHHI")
# scatter.smooth(x=dealdf$PIGHHI, y=dealdf$Deal_Date, main="FIRR - PIGHHI")
# scatter.smooth(x=dealdf$PICHHI, y=dealdf$Deal_Date, main="FIRR - PICHHI")
# scatter.smooth(x=dealdf$PISHHI, y=dealdf$Deal_Date, main="FIRR - PISHHI")


# qplot(data = dealdf, x = dealdf$PISHHI, main="test")








