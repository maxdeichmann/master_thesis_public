# sources
# https://www.princeton.edu/~otorres/Panel101R.pdf
# http://karthur.org/2016/fixed-effects-panel-models-in-r.html
# draw line: http://www.stat.columbia.edu/~martin/W2024/R9.pdf
# entire process: http://sia.webpopix.org/polynomialRegression1.html#fitting-a-polynomial-of-degree-0
# jitools: https://cran.r-project.org/web/packages/jtools/vignettes/summ.html
# tables: https://cran.r-project.org/web/packages/stargazer/vignettes/stargazer.pdf

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
library(jtools) 
library(visreg)
library(stargazer)
library(huxtable)
library(cowplot)
library(openxlsx)
library(plm)
library(gplots)
library(lfe)
library(nlme)
library(patchwork)
library(car)
library(corrplot)
library(knitr)
library(xtable)
require(foreign)
require(MASS) 

# import sources
setwd("/Users/maximiliandeichmann/Development/MasterThesis")
source("helperFunctions.R")

# set wd and load dfs
setwd("/Users/maximiliandeichmann/Development/MasterThesis")
load("dataPreperation_deal.Rda")
load("dataPreperation_fund.Rda")
load("dataPreperation_group.Rda")
load("controlVector.RData")
load("divIndices.RData")
load("fundDivIndices.RData")
load("eiIndices.RData")
load("fundhhiIndices.RData")
load("lfundhhiIndices.RData")
load("fundeiIndices.RData")
load("lfundeiIndices.RData")


# filter for non errors
# is.na(dealdf) <- sapply(dealdf, is.infinite)
# dealdf <- na.omit(dealdf)
# is.na(funddf) <- sapply(funddf, is.infinite)
# subfunddf <- na.omit(funddf)

fund_hhis <- c("Fund_GeoHHI","Fund_StageHHI","Fund_PISHHI","Fund_PIGHHI","Fund_PICHHI")

#--plotting deal level------------------------------------------------------------------------------------------------------
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

# a <- ggplot() + geom_histogram(data=dealdf, aes(GeoHHI)) + theme_minimal()
# b <- ggplot() + geom_histogram(data=dealdf, aes(StageHHI)) + theme_minimal()
# c <- ggplot() + geom_histogram(data=dealdf, aes(PISHHI)) + theme_minimal()
# d <- ggplot() + geom_histogram(data=dealdf, aes(PIGHHI)) + theme_minimal()
# e <- ggplot() + geom_histogram(data=dealdf, aes(PICHHI)) + theme_minimal()
# plot_grid(a,b,c,d,e)
# 
# a <- ggplot() + geom_histogram(data=dealdf, aes(LGeoHHI)) + theme_minimal()
# b <- ggplot() + geom_histogram(data=dealdf, aes(LStageHHI)) + theme_minimal()
# c <- ggplot() + geom_histogram(data=dealdf, aes(LPISHHI)) + theme_minimal()
# d <- ggplot() + geom_histogram(data=dealdf, aes(LPIGHHI)) + theme_minimal()
# e <- ggplot() + geom_histogram(data=dealdf, aes(LPICHHI)) + theme_minimal()
# plot_grid(a,b,c,d,e)
# 
# a <- hist(dealdf$Fund_GeoHHI)#ggplot() + geom_histogram(data=funddf, aes(Fund_GeoHHI)) + theme_minimal()
# b <- hist(dealdf$Fund_StageHHI)#ggplot() + geom_histogram(data=funddf, aes(Fund_StageHHI)) + theme_minimal()
# c <- hist(dealdf$Fund_PIGHHI)#ggplot() + geom_histogram(data=funddf, aes(Fund_PIGHHI)) + theme_minimal()
# d <- hist(dealdf$Fund_PICHHI)#ggplot() + geom_histogram(data=funddf, aes(Fund_PICHHI)) + theme_minimal()
# e <- hist(dealdf$Fund_PISHHI)#ggplot() + geom_histogram(data=funddf, aes(Fund_PISHHI)) + theme_minimal()
# plot_grid(a,b,c,d,e)

# a <- ggplot(dealdf, aes(Deal_Size)) + geom_histogram() + theme_minimal()
# b <- ggplot(dealdf, aes(LDeal_Size)) + geom_histogram() + theme_minimal()
# plot_grid(a,b)

# a <- ggplot(dealdf, aes(Gross_IRR)) + geom_histogram() + theme_minimal()
# b <- ggplot(dealdf, aes(LGross_IRR)) + geom_histogram() + theme_minimal()
# plot_grid(a,b)

# variable test

# outcome <- c()
# for (variable in fund_hhis) {
#   temp <- hist(dealdf[[variable]], prob=T)
#   lines(density(dealdf[[variable]]))
#   outcome <- c(outcome,temp)
# }


# shapiro.test(dealdf$Fund_GeoHHI)
# shapiro.test(dealdf$Fund_StageHHI)
# shapiro.test(dealdf$Fund_PISHHI)
# shapiro.test(dealdf$Fund_PICHHI)
# shapiro.test(dealdf$Fund_PIGHHI)
# shapiro.test(dealdf$Gross_IRR)

scatterTrend("LGross_IRR",lfundhhiIndices, dealdf)




# 
# a <- ggplot(dealdf, aes(Total_Investments)) + geom_histogram() + theme_minimal()
# b <- ggplot(dealdf, aes(Log_Total_Investments)) + geom_histogram() + theme_minimal()
# plot_grid(a,b)
# 
# a <- ggplot(dealdf, aes(Deal_Size)) + geom_histogram() + theme_minimal()
# b <- ggplot(dealdf, aes(LDeal_Size)) + geom_histogram() + theme_minimal()
# plot_grid(a,b)

#--plotting fund level------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
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

# a <- ggplot(funddf, aes(Fund_SD)) + geom_histogram() + theme_minimal()
# b <- ggplot(funddf, aes(LFund_SD)) + geom_histogram() + theme_minimal()
# plot_grid(a,b)

# a <- ggplot(funddf, aes(Fund_GeoHHI)) + geom_histogram() + theme_minimal()
# b <- ggplot(funddf, aes(Fund_StageHHI)) + geom_histogram() + theme_minimal()
# c <- ggplot(funddf, aes(Fund_PISHHI)) + geom_histogram() + theme_minimal()
# d <- ggplot(funddf, aes(Fund_PIGHHI)) + geom_histogram() + theme_minimal()
# e <- ggplot(funddf, aes(Fund_PICHHI)) + geom_histogram() + theme_minimal()
# plot_grid(a,b,c,d,e)

# a <- ggplot(funddf, aes(LFund_GeoHHI)) + geom_histogram() + theme_minimal()
# b <- ggplot(funddf, aes(LFund_StageHHI)) + geom_histogram() + theme_minimal()
# c <- ggplot(funddf, aes(LFund_PISHHI)) + geom_histogram() + theme_minimal()
# d <- ggplot(funddf, aes(LFund_PIGHHI)) + geom_histogram() + theme_minimal()
# e <- ggplot(funddf, aes(LFund_PICHHI)) + geom_histogram() + theme_minimal()
# plot_grid(a,b,c,d,e)

# independent <- c("Fund_SD")#,"Fund_StageHHI","Fund_PISHHI","Fund_PIGHHI","Fund_PICHHI")
# dependent <- "LGross_IRR"
# scatterTrend(dependent,independent,dealdf)




#--correlation matrices-----------------------------------------------------------------------------------------------------

# correlation matrices
# ind <- c(dealdf$GeoHHI, dealdf$StageHHI, dealdf$PIGHHI, dealdf$PISHHI, dealdf$PICHHI)
# ma <- matrix(ind, ncol = 5, nrow = nrow(dealdf))
# cor(dealdf$Gross_IRR, ma)



#RETURN ANALYSIS--------------------------------------------------------------------------------------------------------


# run polynomial analysis
#ols1 <- lm(LGross_IRR~Fund_GeoHHI+Fund_StageHHI+Fund_PIGHHI+as.factor(Deal_Year),data = dealdf)

ols1 <- olsAnalysis(log(Gross_IRR+2)~poly(LFund_GeoHHI,2)+poly(LFund_StageHHI,2)+poly(LFund_PIGHHI,2)+as.factor(Deal_Year),
            dealdf,
            "IRR_ols1.xlsx",
            TRUE)

# outlier
subdf <- subset(dealdf,!(rownames(dealdf) %in% c(395))) # deal_ID: 365

ols2 <- olsAnalysis(log(Gross_IRR+2)~poly(LFund_GeoHHI,2)+poly(LFund_StageHHI,2)+poly(LFund_PIGHHI,2)+as.factor(Deal_Year),
            subdf,
            "IRR_ols2.xlsx",
            TRUE)


ols2 <- lm(log(Gross_IRR+2)~poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+poly(Fund_PIGHHI,2)+Number_Investments+Total_Investments+as.factor(Deal_Year),data = dealdf)
output2 <- huxreg(ols2, statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'adj. R squared' = 'adj.r.squared', 'F statistic' = 'statistic', 'P value' = 'p.value'))
print(output2)
wb1 <- as_Workbook(output2)
openxlsx::saveWorkbook(wb1,"IRR_ols2.xlsx", overwrite = TRUE)
histo(ols2$residuals, "residuals")
# plot(ols2)

# check normality
shapiro.test(ols2$residuals)

# check heteroskedasticity
ncvTest(ols2)

# outlier
subdf <- subset(dealdf,!(rownames(dealdf) %in% c(395)))

ols3 <- lm(log(Gross_IRR+2)~poly(LFund_GeoHHI,2)+poly(LFund_StageHHI,2)+poly(LFund_PIGHHI,2)+Number_Investments+Total_Investments+as.factor(Deal_Year)+as.factor(Fund_ID),data = subdf)
output3 <- huxreg(ols3, statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'adj. R squared' = 'adj.r.squared', 'F statistic' = 'statistic', 'P value' = 'p.value'))
print(output3)
wb3 <- as_Workbook(output3)
openxlsx::saveWorkbook(wb3,"IRR_ols3.xlsx", overwrite = TRUE)
histo(ols3$residuals, "residuals")
plot(ols3)




# check correlation
va <- cbind(dealdf$Fund_GeoHHI, dealdf$Fund_StageHHI, dealdf$Fund_PISHHI, dealdf$Fund_PIGHHI, dealdf$LFund_PICHHI, dealdf$Number_Investments, dealdf$Total_Investments, dealdf$Operating_Years, dealdf$Deal_Year, dealdf$Deal_Size)
na <- c(fund_hhis,"Number_Investments","Total_Investments", "Operating_Years", "Deal_Year", "Deal_Size")
correlation(va,na)


# run regression with controll variabels
ols2 <- lm(LGross_IRR~poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+poly(Fund_PIGHHI,2)+Number_Investments+Total_Investments+Deal_Size+as.factor(Deal_Year),data = dealdf)
output2 <- huxreg(ols2, statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'adj. R squared' = 'adj.r.squared', 'F statistic' = 'statistic', 'P value' = 'p.value'))
print(output2)
wb2 <- as_Workbook(output2)
openxlsx::saveWorkbook(wb2,"IRR_ols2.xlsx", overwrite = TRUE)
histo(ols2$residuals, "residuals")
# plot(ols2)

# vif
vif(ols2)

# check normality
shapiro.test(ols2$residuals)

# check heteroskedasticity
ncvTest(ols2)


# add more fixed effects
ols3 <- lm(LGross_IRR~poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+poly(Fund_PIGHHI,2)+Number_Investments+Total_Investments+Deal_Size+as.factor(Deal_Year),data = dealdf)
fe3 <- felm(LGross_IRR~poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+poly(Fund_PIGHHI,2)+Number_Investments+Total_Investments+Deal_Size|Deal_Year,data = dealdf)
output3 <- huxreg(ols3,fe3, statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'adj. R squared' = 'adj.r.squared', 'F statistic' = 'statistic', 'P value' = 'p.value'))
print(output3)
wb3 <- as_Workbook(output3)
openxlsx::saveWorkbook(wb3,"IRR_ols3.xlsx", overwrite = TRUE)
histo(ols3$residuals, "residuals")
# plot(ols3)

# identify+kill outliers
outlierTest(ols3)
outlier <- outlierTest(ols3)
outliers <- as.numeric(names(outlier[["rstudent"]]))
subdf <-subset(dealdf,!(rownames(dealdf) %in% outliers))

# run analysis again
ols4 <- lm(LGross_IRR~poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+poly(Fund_PIGHHI,2)+Number_Investments+Total_Investments+Deal_Size+as.factor(Deal_Year),data = subdf)
fe4 <- felm(LGross_IRR~poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+poly(Fund_PIGHHI,2)+Number_Investments+Total_Investments+Deal_Size|Deal_Year,data = subdf)
output4 <- huxreg(ols4,fe4, statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'adj. R squared' = 'adj.r.squared', 'F statistic' = 'statistic', 'P value' = 'p.value'))
print(output4)
wb4 <- as_Workbook(output4)
openxlsx::saveWorkbook(wb4, file = "IRR_ols4.xlsx", overwrite = TRUE)
histo(ols4$residuals, "residuals")
# plot(ols4)

# run robust analysis
# https://stats.idre.ucla.edu/r/dae/robust-regression/

# entropy index
fe5 <- felm(LGross_IRR~poly(Fund_GeoEI,2)+poly(Fund_StageEI,2)+poly(Fund_PIGEI,2)+Number_Investments+Total_Investments+Deal_Size|Deal_Year,data = dealdf)
output5 <- huxreg(fe3,fe5, statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'adj. R squared' = 'adj.r.squared', 'F statistic' = 'statistic', 'P value' = 'p.value'))
print(output5)
wb5 <- as_Workbook(output5)
openxlsx::saveWorkbook(wb5,"IRR_ols5.xlsx", overwrite = TRUE)
histo(fe5$residuals, "residuals")


# END OF RETURN ANALYSIS








#Risk ANALYSIS--------------------------------------------------------------------------------------------------------


# ols1 <- lm(Fund_SD~poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+poly(Fund_PIGHHI,2),data = funddf)
# output1 <- huxreg(ols1, statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'adj. R squared' = 'adj.r.squared', 'F statistic' = 'statistic', 'P value' = 'p.value'))
# print(output1)
# wb1 <- as_Workbook(output1)
# openxlsx::saveWorkbook(wb1,"SD_ols1.xlsx", overwrite = TRUE)
# histo(ols1$residuals, "residuals")
# plot(ols1)
# 
# ols2 <- lm(log(Gross_IRR+1/Fund_SD)~poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+poly(Fund_PIGHHI,2)+Number_Investments+Total_Investments+Deal_Size+as.factor(Deal_Year),data = dealdf)
# output2 <- huxreg(ols2, statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'adj. R squared' = 'adj.r.squared', 'F statistic' = 'statistic', 'P value' = 'p.value'))
# print(output2)
# wb2 <- as_Workbook(output2)
# openxlsx::saveWorkbook(wb2,"SD_ols2.xlsx", overwrite = TRUE)
# histo(ols2$residuals, "residuals")
# plot(ols2)
# 
# # check normality
# shapiro.test(ols2$residuals)
# 
# # # check heteroskedasticity
# ncvTest(ols2)

# ols1 <- lm(Fund_SD~poly(LFund_GeoHHI,2)+poly(LFund_StageHHI,2)+poly(LFund_PIGHHI,2)+LNumber_Investments+Deal_Size+as.factor(Deal_Year)+as.factor(Company_Country),data = homodf)
# summary(ols1)
# 
# # put into fe model
# fe <- felm(Fund_SD~poly(LFund_GeoHHI,2)+poly(LFund_StageHHI,2)+poly(LFund_PIGHHI,2)+LNumber_Investments+Deal_Size|Deal_Year+Company_Country, data = homodf)
# summary(fe)
# 
# output1 <- huxreg(ols1,fe, statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'adj. R squared' = 'adj.r.squared', 'F statistic' = 'statistic', 'P value' = 'p.value'))
# print(output1)
# wb1 <- as_Workbook(output1)
# saveWorkbook(wb1,"ols1_SD.xlsx")
# outlierTest(ols1)
# shapiro.test(ols1$residuals)
# ncvTest(ols1)
# vif(ols1)
# plot(ols1)

# run with dealdf and log
# ols2 <- lm(log(Fund_SD)~poly(LGeoHHI,2)+poly(LStageHHI,2)+poly(LPIGHHI,1)+Number_Investments,data = dealdf)
# summary(ols2)
# 
# output2 <- huxreg(ols2, statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'adj. R squared' = 'adj.r.squared', 'F statistic' = 'statistic', 'P value' = 'p.value'))
# print(output2)
# wb2 <- as_Workbook(output2)
# saveWorkbook(wb2,"ols2_SD.xlsx")
# outlier <- outlierTest(ols2)
# shapiro.test(ols2$residuals)
# ncvTest(ols2)
# vif(ols2)
# #plot(ols2)
# 
# outliers <- as.numeric(names(outlier[["rstudent"]]))
# 
# # kill outliers
# subdealdf <- subset(dealdf,!(rownames(dealdf) %in% outliers))
# 
# # run without outliers and logarithmic
# ols3 <- lm(log(Fund_SD)~poly(LGeoHHI,2)+poly(LStageHHI,2)+poly(LPIGHHI,1)+Number_Investments,data = subdealdf)
# summary(ols3)
# plot(ols3)
# 
# output3 <- huxreg(ols3, statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'adj. R squared' = 'adj.r.squared', 'F statistic' = 'statistic', 'P value' = 'p.value'))
# print(output3)
# wb3 <- as_Workbook(output3)
# saveWorkbook(wb3,"ols3_SD.xlsx")
# outlier <- outlierTest(ols3)
# shapiro.test(ols3$residuals)
# ncvTest(ols3)
# vif(ols3)
# plot(ols3)



#Risk/Return ANALYSIS--------------------------------------------------------------------------------------------------------

# ols1 <- lm(LGross_IRR~LFund_SD+as.factor(Deal_Year),data = dealdf)
# output1 <- huxreg(ols1, statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'adj. R squared' = 'adj.r.squared', 'F statistic' = 'statistic', 'P value' = 'p.value'))
# print(output1)
# wb1 <- as_Workbook(output1)
# openxlsx::saveWorkbook(wb1,"SDIRR_ols1.xlsx", overwrite = TRUE)
# histo(ols1$residuals, "residuals")
# scatterTrend("LGross_IRR","LFund_SD",dealdf)
# # plot(ols1)
# 
# # normality tests
# shapiro.test(ols1$residuals)
# ncvTest(ols1)
# 
# # killing value 395
# subdf <- subset(dealdf,!(rownames(dealdf) %in% c(395)))
# 
# ols2 <- lm(LGross_IRR~LFund_SD+as.factor(Deal_Year),data = subdf)
# output2 <- huxreg(ols2, statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'adj. R squared' = 'adj.r.squared', 'F statistic' = 'statistic', 'P value' = 'p.value'))
# print(output2)
# wb2 <- as_Workbook(output2)
# openxlsx::saveWorkbook(wb1,"SDIRR_ols2.xlsx", overwrite = TRUE)
# histo(ols2$residuals, "residuals")
# plot(ols2)
# 
# # normality tests
# shapiro.test(ols2$residuals)
# ncvTest(ols2)




# exlude
# identify+kill outliers
# outlier <- outlierTest(ols2)
# outliers <- as.numeric(names(outlier[["rstudent"]]))
# subdf2 <-subset(dealdf,!(rownames(dealdf) %in% outliers))
# subdf1 <- subset(dealdf,!(rownames(dealdf) %in% c(395)))
