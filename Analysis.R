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

fund_hhis <- c("Fund_GeoHHI","Fund_StageHHI","Fund_PISHHI","Fund_PICHHI","Fund_PIGHHI")

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

# independent variables

# a <- ggplot() + geom_histogram(data=dealdf, aes(GeoHHI)) + theme_minimal()
# b <- ggplot() + geom_histogram(data=dealdf, aes(StageHHI)) + theme_minimal()
# c <- ggplot() + geom_histogram(data=dealdf, aes(PIGHHI)) + theme_minimal()
# d <- ggplot() + geom_histogram(data=dealdf, aes(PICHHI)) + theme_minimal()
# e <- ggplot() + geom_histogram(data=dealdf, aes(PISHHI)) + theme_minimal()
# plot_grid(a,b,c,d,e)
# 
# a <- ggplot() + geom_histogram(data=dealdf, aes(LGeoHHI)) + theme_minimal()
# b <- ggplot() + geom_histogram(data=dealdf, aes(LStageHHI)) + theme_minimal()
# c <- ggplot() + geom_histogram(data=dealdf, aes(LPIGHHI)) + theme_minimal()
# d <- ggplot() + geom_histogram(data=dealdf, aes(LPICHHI)) + theme_minimal()
# e <- ggplot() + geom_histogram(data=dealdf, aes(LPISHHI)) + theme_minimal()
# plot_grid(a,b,c,d,e)
# 
# a <- hist(dealdf$Fund_GeoHHI)#ggplot() + geom_histogram(data=funddf, aes(Fund_GeoHHI)) + theme_minimal()
# b <- hist(dealdf$Fund_StageHHI)#ggplot() + geom_histogram(data=funddf, aes(Fund_StageHHI)) + theme_minimal()
# c <- hist(dealdf$Fund_PIGHHI)#ggplot() + geom_histogram(data=funddf, aes(Fund_PIGHHI)) + theme_minimal()
# d <- hist(dealdf$Fund_PICHHI)#ggplot() + geom_histogram(data=funddf, aes(Fund_PICHHI)) + theme_minimal()
# e <- hist(dealdf$Fund_PISHHI)#ggplot() + geom_histogram(data=funddf, aes(Fund_PISHHI)) + theme_minimal()
# plot_grid(a,b,c,d,e)

# test for normal distribution

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
# outcome <- list()
# dependent <- "Gross_IRR"
# for (hhi in fund_hhis) {
#   temp <- ggplot(data = dealdf,aes(dealdf[[hhi]],dealdf[[dependent]])) + theme_minimal() + 
#     geom_point() + geom_smooth(method = "lm",formula=y ~ poly(x, 2, raw=TRUE)) + 
#     xlab(hhi) + ylab(dependent)
#   outcome[[hhi]] <- temp
# }
# wrap_plots(outcome)



# 
# a <- ggplot() + geom_histogram(data=funddf, aes(LFund_GeoHHI)) + theme_minimal()
# b <- ggplot() + geom_histogram(data=funddf, aes(LFund_StageHHI)) + theme_minimal()
# c <- ggplot() + geom_histogram(data=funddf, aes(LFund_PIGHHI)) + theme_minimal()
# d <- ggplot() + geom_histogram(data=funddf, aes(LFund_PICHHI)) + theme_minimal()
# e <- ggplot() + geom_histogram(data=funddf, aes(LFund_PISHHI)) + theme_minimal()
# plot_grid(a,b,c,d,e)
# 
# a <- ggplot(dealdf, aes(Total_Investments)) + geom_histogram() + theme_minimal()
# b <- ggplot(dealdf, aes(Log_Total_Investments)) + geom_histogram() + theme_minimal()
# plot_grid(a,b)
# 
# a <- ggplot(dealdf, aes(Deal_Size)) + geom_histogram() + theme_minimal()
# b <- ggplot(dealdf, aes(LDeal_Size)) + geom_histogram() + theme_minimal()
# plot_grid(a,b)

# Plotting Fund level
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

# dependent variables
# a <- ggplot(dealdf, aes(Gross_IRR)) + geom_histogram() + theme_minimal()
# b <- ggplot(dealdf, aes(LGross_IRR)) + geom_histogram() + theme_minimal()
# plot_grid(a,b)

# a <- ggplot(funddf, aes(Fund_SD)) + geom_histogram() + theme_minimal()
# b <- ggplot(funddf, aes(LFund_SD)) + geom_histogram() + theme_minimal()
# plot_grid(a,b)

# hhis <- c("Geo_HHI","Stage_HHI","PICHHI","PISHHI","PIGHHI")
# outcome <- list()
# dependent <- "Fund_SD"
# for (hhi in hhis) {
#   temp <- ggplot(data = dealdf,aes(dealdf[[hhi]],dealdf[[dependent]])) + theme_minimal() +
#     geom_point() + geom_smooth(method = "lm",formula=y ~ poly(x, 2, raw=TRUE)) +
#     xlab(hhi) + ylab(dependent)
#   outcome[[hhi]] <- temp
# }
# wrap_plots(outcome)

#--correlation matrices-----------------------------------------------------------------------------------------------------

# correlation matrices
# ind <- c(dealdf$GeoHHI, dealdf$StageHHI, dealdf$PIGHHI, dealdf$PISHHI, dealdf$PICHHI)
# ma <- matrix(ind, ncol = 5, nrow = nrow(dealdf))
# cor(dealdf$Gross_IRR, ma)
# 
# ind <- c(subfunddf$Fund_GeoHHI, subfunddf$Fund_StageHHI, subfunddf$Fund_PIGHHI, subfunddf$Fund_PISHHI, subfunddf$Fund_PICHHI)
# ma <- matrix(ind, ncol = 5, nrow = nrow(subfunddf))
# cor(subfunddf$Fund_SD, ma)


#RETURN ANALYSIS--------------------------------------------------------------------------------------------------------

ols <- lm(Gross_IRR~Fund_GeoHHI+Fund_StageHHI+Fund_PISHHI+Number_Investments+Total_Investments+as.factor(Deal_Year),data = dealdf)
# output <- huxreg(ols, statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'adj. R squared' = 'adj.r.squared', 'F statistic' = 'statistic', 'P value' = 'p.value'))
# print(output)
# wb <- as_Workbook(output)
# saveWorkbook(wb,"ols.xlsx")
# # plot(ols)
# #
# #
# #
# 
# 
# # identify outliers
# outlier <- outlierTest(ols)
# outliers <- as.numeric(names(outlier[["rstudent"]]))
# 
# # # kill outliers
# subdf <- subset(dealdf,!(rownames(dealdf) %in% outliers))
# 
# # run analysis again
# ols2 <- lm(Gross_IRR~Fund_GeoHHI+Fund_StageHHI+Fund_PISHHI+Number_Investments+Total_Investments+as.factor(Deal_Year),data = subdf)
# output2 <- huxreg(ols2, statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'adj. R squared' = 'adj.r.squared', 'F statistic' = 'statistic', 'P value' = 'p.value'))
# print(output2)
# wb2 <- as_Workbook(output2)
# saveWorkbook(wb2,"ols2.xlsx")
# outlierTest(ols2)
# # plot(ols2)
# #
# #
# #
# 
# # run polynomial analysis
# ols3 <- lm(Gross_IRR~poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+poly(Fund_PISHHI,2)+Number_Investments+Total_Investments+as.factor(Deal_Year),data = subdf)
# output3 <- huxreg(ols3, statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'adj. R squared' = 'adj.r.squared', 'F statistic' = 'statistic', 'P value' = 'p.value'))
# print(output3)
# wb3 <- as_Workbook(output3)
# saveWorkbook(wb3,"ols3.xlsx")
# outlierTest(ols3)
# #plot(ols3)
# 
# # reducing even more outliers
# # identify outliers
# outlier <- outlierTest(ols3)
# outliers <- as.numeric(names(outlier[["rstudent"]]))
# 
# # transform to normally distribute residuals
# 
# ols4 <- lm(LGross_IRR~poly(LFund_GeoHHI,2)+poly(LFund_StageHHI,2)+poly(LFund_PISHHI,2)+LNumber_Investments+LTotal_Investments+as.factor(Deal_Year),data = subdf)
# output4 <- huxreg(ols4, statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'adj. R squared' = 'adj.r.squared', 'F statistic' = 'statistic', 'P value' = 'p.value'))
# print(output4)
# wb4 <- as_Workbook(output4)
# saveWorkbook(wb4,"ols4.xlsx")
# outlierTest(ols4)
# shapiro.test(ols4$residuals)
# #plot(ols4)
# 
# 
# # test multicolinearity
# 
# #1. correlation table
# variables <- cbind(subdf$LFund_GeoHHI, subdf$LFund_StageHHI, subdf$LFund_PIGHHI, subdf$LFund_PISHHI, subdf$LFund_PICHHI, subdf$LNumber_Investments, subdf$LTotal_Investments)
# colnames(variables) <- c(fund_hhis,"LNumber_Investments","LTotal_Investments")
# M <- cor(variables)
# p.mat <- cor.mtest(variables)
# col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
# corrplot(M, method="color", col=col(200),
#          type="upper", order="hclust",
#          addCoef.col = "black", # Add coefficient of correlation
#          tl.col="black", tl.srt=45, #Text label color and rotation
#          # Combine with significance
#          p.mat = p.mat, sig.level = 0.01, insig = "blank",
#          # hide correlation coefficient on the principal diagonal
#          diag=FALSE
# )
# #2. VIF
# vif(ols4)
# 
# # improved model
# # ols5 <- lm(LGross_IRR~poly(LFund_GeoHHI,2)+poly(LFund_StageHHI,2)+poly(LFund_PISHHI,2)+LNumber_Investments +as.factor(Deal_Year),data = subdf)
# # output5 <- huxreg(ols5, statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'adj. R squared' = 'adj.r.squared', 'F statistic' = 'statistic', 'P value' = 'p.value'))
# # print(output5)
# # wb5 <- as_Workbook(output5)
# # saveWorkbook(wb5,"ols5.xlsx")
# # outlierTest(ols5)
# # shapiro.test(ols5$residuals)
# # plot(ols5)
# 
# 
# # heteroskedastizity
# ncvTest(ols4)
# #spreadLevelPlot(ols4)
# homodf <- subset(subdf,!(rownames(subdf) %in% c(395)))
# 
# ols6 <- lm(LGross_IRR~poly(LFund_GeoHHI,2)+poly(LFund_StageHHI,2)+poly(LFund_PISHHI,2)+LNumber_Investments +as.factor(Deal_Year),data = homodf)
# output6 <- huxreg(ols6, statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'adj. R squared' = 'adj.r.squared', 'F statistic' = 'statistic', 'P value' = 'p.value'))
# print(output6)
# wb6 <- as_Workbook(output6)
# saveWorkbook(wb6,"ols6.xlsx")
# outlierTest(ols6)
# shapiro.test(ols6$residuals)
# ncvTest(ols6)
# #plot(ols6)
# 
# # include more controll variables
# ols7 <- lm(LGross_IRR~poly(LFund_GeoHHI,2)+poly(LFund_StageHHI,2)+poly(LFund_PISHHI,2)+LNumber_Investments+Operating_Years+Deal_Size+as.factor(Deal_Year),data = homodf)
# output7 <- huxreg(ols7, statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'adj. R squared' = 'adj.r.squared', 'F statistic' = 'statistic', 'P value' = 'p.value'))
# print(output7)
# wb7 <- as_Workbook(output7)
# saveWorkbook(wb7,"ols7.xlsx")
# outlierTest(ols7)
# shapiro.test(ols7$residuals)
# ncvTest(ols7)
# #plot(ols7)
# 
# # correlation check
# #1. correlation table
# variables <- cbind(homodf$LFund_GeoHHI, homodf$LFund_StageHHI, homodf$LFund_PIGHHI, homodf$LFund_PISHHI, homodf$LFund_PICHHI, homodf$LNumber_Investments, homodf$LTotal_Investments, homodf$Deal_Size, homodf$Operating_Years)
# colnames(variables) <- c(fund_hhis,"LNumber_Investments","LTotal_Investments", "Deal_Size", "Operating_Years")
# M <- cor(variables)
# p.mat <- cor.mtest(variables)
# col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
# corrplot(M, method="color", col=col(200),
#          type="upper", order="hclust",
#          addCoef.col = "black", # Add coefficient of correlation
#          tl.col="black", tl.srt=45, #Text label color and rotation
#          # Combine with significance
#          p.mat = p.mat, sig.level = 0.01, insig = "blank",
#          # hide correlation coefficient on the principal diagonal
#          diag=FALSE
# )
# #2. VIF
# vif(ols7)
# 
# # remove operating years
# ols8 <- lm(LGross_IRR~poly(LFund_GeoHHI,2)+poly(LFund_StageHHI,2)+poly(LFund_PISHHI,2)+LNumber_Investments+Deal_Size+as.factor(Deal_Year),data = homodf)
# output8 <- huxreg(ols8, statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'adj. R squared' = 'adj.r.squared', 'F statistic' = 'statistic', 'P value' = 'p.value'))
# print(output8)
# wb8 <- as_Workbook(output8)
# saveWorkbook(wb8,"ols8.xlsx")
# outlierTest(ols8)
# shapiro.test(ols8$residuals)
# ncvTest(ols8)
# # plot(ols8)
# 
# # add FE
# # PIS -> 0.06049
# # PIC -> 0.06553
# # PIG -> 0.05719
# ols9 <- lm(LGross_IRR~poly(LFund_GeoHHI,2)+poly(LFund_StageHHI,2)+poly(LFund_PIGHHI,2)+LNumber_Investments+Deal_Size+as.factor(Deal_Year)+as.factor(Company_Country),data = homodf)
# summary(ols9)
# 
# # put into fe model
# fe <- felm(LGross_IRR~poly(LFund_GeoHHI,2)+poly(LFund_StageHHI,2)+poly(LFund_PIGHHI,2)+LNumber_Investments+Deal_Size|Deal_Year+Company_Country, data = homodf)
# summary(fe)
# 
# output9 <- huxreg(ols9,fe, statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'adj. R squared' = 'adj.r.squared', 'F statistic' = 'statistic', 'P value' = 'p.value'))
# print(output9)
# wb9 <- as_Workbook(output9)
# saveWorkbook(wb9,"ols9.xlsx")
# outlierTest(ols9)
# shapiro.test(ols9$residuals)
# ncvTest(ols9)
# vif(ols9)
# # plot(ols9)
# 
# source("helperFunctions.R")
# plotModel1(ols9,homodf,"LGross_IRR",c("LFund_GeoHHI","LFund_StageHHI","LFund_PIGHHI"),c(0,1),"HHI","Model1")
# effect_plot(ols9, pred = LFund_GeoHHI,data = homodf)

# END OF RETURN ANALYSIS








#Risk ANALYSIS--------------------------------------------------------------------------------------------------------

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
ols2 <- lm(Fund_SD~GeoHHI+StageHHI+PIGHHI+Number_Investments,data = dealdf)
summary(ols2)

output2 <- huxreg(ols2, statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'adj. R squared' = 'adj.r.squared', 'F statistic' = 'statistic', 'P value' = 'p.value'))
print(output2)
wb2 <- as_Workbook(output2)
saveWorkbook(wb2,"ols2_SD.xlsx")
outlier <- outlierTest(ols2)
shapiro.test(ols2$residuals)
ncvTest(ols2)
vif(ols2)
#plot(ols2)

outliers <- as.numeric(names(outlier[["rstudent"]]))

# kill outliers
subdealdf <- subset(dealdf,!(rownames(dealdf) %in% outliers))

# run without outliers and logarithmic
ols3 <- lm(Fund_SD~GeoHHI+StageHHI+PIGHHI+Number_Investments,data = subdealdf)
summary(ols3)


output3 <- huxreg(ols3, statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'adj. R squared' = 'adj.r.squared', 'F statistic' = 'statistic', 'P value' = 'p.value'))
print(output3)
wb3 <- as_Workbook(output3)
saveWorkbook(wb3,"ols3_SD.xlsx")
outlier <- outlierTest(ols3)
shapiro.test(ols3$residuals)
ncvTest(ols3)
vif(ols3)
plot(ols3)


