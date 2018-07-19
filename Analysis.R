# sources
# https://www.princeton.edu/~otorres/Panel101R.pdf


## The option below is used upon initializing the rJava environment
## and may be placed in an .Renviron or .Rprofile file
options(java.parameters = c("-Djava.awt.headless=true", "-Xmx1g"))

#clear console
cat("\014")

#clear workspace
rm(list = ls())

#clear graphs
graphics.off()

#Import required libraries
library(plyr)
library(ggplot2)
library(jtools) #https://cran.r-project.org/web/packages/jtools/vignettes/summ.html
library(visreg)
library(stargazer)
library(huxtable)
library(cowplot)
library(openxlsx)
library(plm)
library(gplots)

# import sources
setwd("/Users/maximiliandeichmann/Development/MasterThesis")
source("helperFunctions.R")

# set wd and load dfs
setwd("/Users/maximiliandeichmann/Development/MasterThesis")
load("dataPreperation_deal.Rda")
load("dataPreperation_fund.Rda")
load("dataPreperation_group.Rda")
load("controlVector.RData")

# filter for non errors
is.na(dealdf) <- sapply(dealdf, is.infinite)
dealdf <- na.omit(dealdf)
# is.na(funddf) <- sapply(funddf, is.infinite)
# subfunddf <- na.omit(funddf)

#--------------------------------------------------------------------------------------------------------
# stage distribution
# barplot(table(dealdf$Company_Stage))
# ggplot(data=dealdf, aes(x=Company_Stage)) +
#   theme_minimal() +
#   geom_bar()

# group analysis
# ggplot(groupdf, aes(bucket, GeoHHI)) + geom_point() + theme_minimal() + geom_smooth()
# ggplot(groupdf, aes(bucket, StageHHI)) + geom_point() + theme_minimal() + geom_smooth()
# ggplot(groupdf, aes(bucket, PIGHHI)) + geom_point() + theme_minimal() + geom_smooth()
# ggplot(groupdf, aes(bucket, PICHHI)) + geom_point() + theme_minimal() + geom_smooth()
# ggplot(groupdf, aes(bucket, PISHHI)) + geom_point() + theme_minimal() + geom_smooth()

# independent variables

# a <- ggplot() + geom_histogram(data=dealdf, aes(GeoHHI)) + theme_minimal()
# b <- ggplot() + geom_histogram(data=dealdf, aes(StageHHI)) + theme_minimal()
# c <- ggplot() + geom_histogram(data=dealdf, aes(PIGHHI)) + theme_minimal()
# d <- ggplot() + geom_histogram(data=dealdf, aes(PICHHI)) + theme_minimal()
# e <- ggplot() + geom_histogram(data=dealdf, aes(PISHHI)) + theme_minimal()
# plot_grid(a,b,c,d,e)

# a <- ggplot() + geom_histogram(data=dealdf, aes(LGeoHHI)) + theme_minimal()
# b <- ggplot() + geom_histogram(data=dealdf, aes(LStageHHI)) + theme_minimal()
# c <- ggplot() + geom_histogram(data=dealdf, aes(LPIGHHI)) + theme_minimal()
# d <- ggplot() + geom_histogram(data=dealdf, aes(LPICHHI)) + theme_minimal()
# e <- ggplot() + geom_histogram(data=dealdf, aes(LPISHHI)) + theme_minimal()
# plot_grid(a,b,c,d,e)

# a <- ggplot() + geom_histogram(data=funddf, aes(Fund_GeoHHI)) + theme_minimal()
# b <- ggplot() + geom_histogram(data=funddf, aes(Fund_StageHHI)) + theme_minimal()
# c <- ggplot() + geom_histogram(data=funddf, aes(Fund_PIGHHI)) + theme_minimal()
# d <- ggplot() + geom_histogram(data=funddf, aes(Fund_PICHHI)) + theme_minimal()
# e <- ggplot() + geom_histogram(data=funddf, aes(Fund_PISHHI)) + theme_minimal()
# print(plot_grid(a,b,c,d,e))

  
# ggplot(dealdf, aes(GeoHHI)) + geom_histogram() + theme_minimal()
# ggplot(dealdf, aes(StageHHI)) + geom_histogram() + theme_minimal()
# ggplot(dealdf, aes(PIGHHI)) + geom_histogram() + theme_minimal()
# ggplot(dealdf, aes(PICHHI)) + geom_histogram() + theme_minimal()
# ggplot(dealdf, aes(PISHHI)) + geom_histogram() + theme_minimal()

# ggplot(dealdf, aes(glog)) + geom_histogram() + theme_minimal()
# ggplot(dealdf, aes(slog)) + geom_histogram() + theme_minimal()
# ggplot(dealdf, aes(piglog)) + geom_histogram() + theme_minimal()
# ggplot(dealdf, aes(piclog)) + geom_histogram() + theme_minimal()
# ggplot(dealdf, aes(pislog)) + geom_histogram() + theme_minimal()

# a <- ggplot(dealdf, aes(Total_Investments)) + geom_histogram() + theme_minimal()
# b <- ggplot(dealdf, aes(Log_Total_Investments)) + geom_histogram() + theme_minimal()
# plot_grid(a,b)
# 
# a <- ggplot(dealdf, aes(Deal_Size)) + geom_histogram() + theme_minimal()
# b <- ggplot(dealdf, aes(LDeal_Size)) + geom_histogram() + theme_minimal()
# plot_grid(a,b)

#--------------------------------------------------------------------------------------------------------
# fund level
# a <- ggplot(funddf, aes(Number_Investments)) + geom_histogram() + theme_minimal()
# b <- ggplot() + geom_histogram(data=funddf, aes(LNumber_Investments)) + theme_minimal()
# plot_grid(a,b)

# a <- ggplot(funddf, aes(Total_Investments)) + geom_histogram() + theme_minimal()
# b <- ggplot() + geom_histogram(data=funddf, aes(LTotal_Investments)) + theme_minimal()
# plot_grid(a,b)

# a <- ggplot() + geom_histogram(data=funddf, aes(Operating_Years)) + theme_minimal()
# b <- ggplot() + geom_histogram(data=funddf, aes(LOperating_Years)) + theme_minimal()
# plot_grid(a,b)

# a <- ggplot() + geom_histogram(data=funddf, aes(Total_Investments)) + theme_minimal()
# b <- ggplot() + geom_histogram(data=funddf, aes(LTotal_Investments)) + theme_minimal()
# plot_grid(a,b)

# dependent variables
# a <- ggplot(dealdf, aes(Gross_IRR)) + geom_histogram() + theme_minimal()
# b <- ggplot(dealdf, aes(LGross_IRR)) + geom_histogram() + theme_minimal()
# plot_grid(a,b)

# a <- ggplot(funddf, aes(Fund_SD)) + geom_histogram() + theme_minimal()
# b <- ggplot(funddf, aes(LFund_SD)) + geom_histogram() + theme_minimal()
# plot_grid(a,b)

# independent on dependent
# ggplot(dealdf, aes(StageHHI, Total_Return)) + geom_point() + geom_smooth() + theme_minimal()
# ggplot(dealdf, aes(GeoHHI, Total_Return)) + geom_point() + geom_smooth() + theme_minimal()
# ggplot(dealdf, aes(PIGHHI, Total_Return)) + geom_point() + geom_smooth() + theme_minimal()
# ggplot(dealdf, aes(PICHHI, Total_Return)) + geom_point() + geom_smooth() + theme_minimal()
# ggplot(dealdf, aes(PISHHI, Total_Return)) + geom_point() + geom_smooth() + theme_minimal()

# ggplot(dealdf, aes(SD_Fund_StageHHI, Fund_SD)) + geom_point() + geom_smooth() + theme_minimal()
# ggplot(dealdf, aes(SD_Fund_GeoHHI, Fund_SD)) + geom_point() + geom_smooth() + theme_minimal()
# ggplot(dealdf, aes(SD_Fund_PIGHHI, Fund_SD)) + geom_point() + geom_smooth() + theme_minimal()
# ggplot(dealdf, aes(SD_Fund_PICHHI, Fund_SD)) + geom_point() + geom_smooth() + theme_minimal()
# ggplot(dealdf, aes(SD_Fund_PISHHI, Fund_SD)) + geom_point() + geom_smooth() + theme_minimal()

#--------------------------------------------------------------------------------------------------------

# correlation matrices
# ind <- c(dealdf$GeoHHI, dealdf$StageHHI, dealdf$PIGHHI, dealdf$PISHHI, dealdf$PICHHI)
# ma <- matrix(ind, ncol = 5, nrow = nrow(dealdf))
# cor(dealdf$Gross_IRR, ma)
# 
# ind <- c(subfunddf$Fund_GeoHHI, subfunddf$Fund_StageHHI, subfunddf$Fund_PIGHHI, subfunddf$Fund_PISHHI, subfunddf$Fund_PICHHI)
# ma <- matrix(ind, ncol = 5, nrow = nrow(subfunddf))
# cor(subfunddf$Fund_SD, ma)

#--------------------------------------------------------------------------------------------------------

# add variables to control vector
# controlVector <- c("LNumber_Investments","LTotal_Investments", controlVector)

# r^2 of 0,1028 @ irr interval @ 0.05 - 0.95
#f1 <- formula(paste("Gross_IRR ~ Fund_GeoHHI + Fund_StageHHI + Fund_PISHHI + ", paste(controlVector, collapse=" + ")))
# r^2 of 12,38 @ irr + deal size interval of 0.01 + 0.9
# f1 <- formula(paste("StageHHI ~ poly(bucket,2)"))
# r^2 of 10; very high acceptance of variables
# f1 <- formula(paste("Fund_SD ~ Fund_PIGHHI + Fund_StageHHI + Fund_GeoHHI +", paste(controlVector, collapse=" + ")))
# f2 <- formula(paste("Fund_SD ~ poly(Fund_PIGHHI,2) + poly(Fund_StageHHI,2) + poly(Fund_GeoHHI,2) +", paste(controlVector, collapse=" + ")))

#f1 <- formula(paste("SD_Gross_IRR ~ poly(Fund_PIGHHI,2) + poly(Fund_StageHHI,2) + poly(Fund_GeoHHI,2) +", paste(controlVector, collapse=" + ")))

# plot_summs(model1, scale = TRUE)
# export_summs(model1)

# summary(model1)

# # check all data for IRR - FUND INDEPENDENT
# f1 <- formula(paste("LGross_IRR ~ poly(LFund_PICHHI,2)+poly(LFund_StageHHI,2)+poly(LFund_GeoHHI,2) + ", paste(controlVector, collapse=" + ")))
# model1 <- lm(f1, data = dealdf)
# a <- effect_plot(model1, pred = Fund_PICHHI, data = dealdf)
# b <- effect_plot(model1, pred = Fund_StageHHI, data = dealdf)
# c <- effect_plot(model1, pred = Fund_GeoHHI, data = dealdf)
# plot_grid(a,b,c, labels = c("1","1","1"))
# 
# # check late stage for IRR
# latedf<- dealdf[dealdf$Company_Stage == "Late Stage",]
# f2 <- formula(paste("LGross_IRR ~ poly(LFund_PICHHI,2)+poly(LFund_GeoHHI,2) + ", paste(controlVector, collapse=" + ")))
# model2 <- lm(f2,data = latedf)
# a <- effect_plot(model2, pred = Fund_PICHHI, data = dealdf)
# b <- effect_plot(model2, pred = Fund_GeoHHI, data = dealdf)
# plot_grid(a,b, labels = c("2","2"))
# 
# # check early stage for IRR
# earlydf<- dealdf[dealdf$Company_Stage == "Early Stage",]
# f3 <- formula(paste("LGross_IRR ~ poly(LFund_PICHHI,2)+poly(LFund_GeoHHI,2) + ", paste(controlVector, collapse=" + ")))
# model3 <- lm(f3,data = earlydf)
# a <- effect_plot(model3, pred = Fund_PICHHI, data = dealdf)
# b <- effect_plot(model3, pred = Fund_GeoHHI, data = dealdf)
# plot_grid(a,b, labels = c("3","3"))
# 
# output <- huxreg("Gross_IRR" = model1, "Gross_IRR - LS" = model2,  "Gross_IRR - ES" = model3, error_pos = 'right', statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'F statistic' = 'statistic','P value' = 'p.value'))
# output <- theme_article(output, header_row = TRUE, header_col = TRUE)
# print(output)
# quick_xlsx(output, file = "huxtable-output.xlsx", borders = 0.4, open = interactive())

# check all data for IRR - DEAL INDEPENDENT
# f1 <- formula(paste("LGross_IRR ~ poly(PICHHI,2)+poly(StageHHI,2)+poly(GeoHHI,2) + ", paste(controlVector, collapse=" + ")))
# model1 <- lm(f1, data = dealdf)
# a <- effect_plot(model1, pred = Fund_PICHHI, data = dealdf)
# b <- effect_plot(model1, pred = Fund_StageHHI, data = dealdf)
# c <- effect_plot(model1, pred = Fund_GeoHHI, data = dealdf)
# plot_grid(a,b,c, labels = c("1","1","1"))
# 
# # check late stage for IRR
# latedf<- dealdf[dealdf$Company_Stage == "Late Stage",]
# f2 <- formula(paste("LGross_IRR ~ poly(PICHHI,2)+poly(GeoHHI,2) + ", paste(controlVector, collapse=" + ")))
# model2 <- lm(f2,data = latedf)
# a <- effect_plot(model2, pred = Fund_PICHHI, data = dealdf)
# b <- effect_plot(model2, pred = Fund_GeoHHI, data = dealdf)
# plot_grid(a,b, labels = c("2","2"))
# 
# # check early stage for IRR
# earlydf<- dealdf[dealdf$Company_Stage == "Early Stage",]
# f3 <- formula(paste("LGross_IRR ~ poly(PICHHI,2)+poly(GeoHHI,2) + ", paste(controlVector, collapse=" + ")))
# model3 <- lm(f3,data = earlydf)
# a <- effect_plot(model3, pred = Fund_PICHHI, data = dealdf)
# b <- effect_plot(model3, pred = Fund_GeoHHI, data = dealdf)
# plot_grid(a,b, labels = c("3","3"))
# 
# output <- huxreg("Gross_IRR" = model1, "Gross_IRR - LS" = model2,  "Gross_IRR - ES" = model3, error_pos = 'right', statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'F statistic' = 'statistic','P value' = 'p.value'))
# output <- theme_article(output, header_row = TRUE, header_col = TRUE)
# print(output)
# quick_xlsx(output, file = "huxtable-output.xlsx", borders = 0.4, open = interactive())

# FIXED EFFECTS


ols <- lm(LGross_IRR~poly(LFund_GeoHHI,2)+poly(LFund_StageHHI,2)+ poly(LFund_PISHHI,2)+LNumber_Investments+LTotal_Investments+as.factor(Deal_Year),data = dealdf)
fe <- plm(LGross_IRR~poly(LFund_GeoHHI,2)+poly(LFund_StageHHI,2)+ poly(LFund_PISHHI,2)+LNumber_Investments+LTotal_Investments, data = dealdf, index = c("Deal_Year","Company_Country"), model = "within") #FE
re <- plm(LGross_IRR~poly(LFund_GeoHHI,2)+poly(LFund_StageHHI,2)+ poly(LFund_PISHHI,2)+LNumber_Investments+LTotal_Investments, data = dealdf, index = c("Deal_Year"), model = "random") #RE
output <- huxreg("OLS" = ols, "FE" = fe, "RE" = re)
phtest(fe, re)
print(output)


# check SD
# toPredict <- c("poly(Fund_PIGHHI, 2)","poly(Fund_StageHHI, 2)","poly(Fund_GeoHHI, 2)")
# independent <- c("Fund_PIGHHI","Fund_StageHHI","Fund_GeoHHI")
# dependent <- "Fund_SD"
# f4 <- formula("Fund_SD ~ poly(Fund_PIGHHI, 2)+poly(Fund_StageHHI, 2)+poly(Fund_GeoHHI, 2)")
# model4 <- lm(f4,data = funddf)
# #plotModel(model4,funddf,dependent2,independent2,controlVector,toPredict2,c(0,0.5),"HHI",dependent2,"Model4")
# 
# # check late stage for SD
# latedf<- dealdf[dealdf$Company_Stage == "Late Stage",]
# toPredict <- c("poly(Fund_PIGHHI, 2)","poly(Fund_StageHHI, 2)","poly(Fund_GeoHHI, 2)")
# independent <- c("Fund_PIGHHI","Fund_StageHHI","Fund_GeoHHI")
# dependent <- "Fund_SD"
# f5 <- formula("Fund_SD ~ poly(Fund_PIGHHI, 2)+poly(Fund_StageHHI, 2)+poly(Fund_GeoHHI, 2)")
# model5 <- lm(f4,data = funddf)
# 
# # check early stage for SD
# earlydf<- dealdf[dealdf$Company_Stage == "Early Stage",]
# toPredict <- c("poly(Fund_PIGHHI, 2)","poly(Fund_StageHHI, 2)","poly(Fund_GeoHHI, 2)")
# independent <- c("Fund_PIGHHI","Fund_StageHHI","Fund_GeoHHI")
# dependent <- "Fund_SD"
# f6 <- formula("Fund_SD ~ poly(Fund_PIGHHI, 2)+poly(Fund_StageHHI, 2)+poly(Fund_GeoHHI, 2)")
# model6 <- lm(f4,data = funddf)

# output <- huxreg("Gross_IRR" = model1, "Gross_IRR - LS" = model2,  "Gross_IRR - ES" = model3, "Fund_SD" = model4, "Fund_SD - LS" = model5, "Fund_SD - ES" = model6)
# print(output)




# fund level analysis
#hhis <- c("GeoHHI","StageHHI","PIGHHI","PICHHI","PISHHI")
#dependent <- "Fund_SD"
#analysis(dependent, hhis, funddf)



# OLD
# # check all data for IRR
# toPredict <- c("poly(Fund_PICHHI, 2)","poly(Fund_StageHHI, 2)","poly(Fund_GeoHHI, 2)")
# independent <- c("Fund_PICHHI","Fund_StageHHI","Fund_GeoHHI")
# dependent <- "Gross_IRR"
# f1 <- formula(paste("LGross_IRR ~ poly(Fund_PICHHI,2)+poly(Fund_StageHHI,2)+poly(Fund_GeoHHI,2) + ", paste(controlVector, collapse=" + ")))
# model1 <- lm(f1, data = dealdf)
# # plotModel(model1,dealdf,dependent,independent,controlVector,toPredict,c(0,1),"HHI",dependent,"Model1")
# a <- effect_plot(model1, pred = Fund_PICHHI, data = dealdf)
# b <- effect_plot(model1, pred = Fund_StageHHI, data = dealdf)
# c <- effect_plot(model1, pred = Fund_GeoHHI, data = dealdf)
# plot_grid(a,b,c, labels = c("1","1","1"))
