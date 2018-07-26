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

#--correlation matrices-----------------------------------------------------------------------------------------------------

# correlation matrices
# ind <- c(dealdf$GeoHHI, dealdf$StageHHI, dealdf$PIGHHI, dealdf$PISHHI, dealdf$PICHHI)
# ma <- matrix(ind, ncol = 5, nrow = nrow(dealdf))
# cor(dealdf$Gross_IRR, ma)
# 
# ind <- c(subfunddf$Fund_GeoHHI, subfunddf$Fund_StageHHI, subfunddf$Fund_PIGHHI, subfunddf$Fund_PISHHI, subfunddf$Fund_PICHHI)
# ma <- matrix(ind, ncol = 5, nrow = nrow(subfunddf))
# cor(subfunddf$Fund_SD, ma)

#--FE models--------------------------------------------------------------------------------------------------------

# FIXED EFFECTS

#BEST:
# ols <- lm(LGross_IRR~poly(LFund_GeoHHI,2)+poly(LFund_StageHHI,2)+ poly(LFund_PISHHI,2)+LNumber_Investments+LTotal_Investments+as.factor(Deal_Year)+as.factor(Company_Country)+as.factor(Primary_Industry_Code),data = dealdf)
# ols <- lm(Gross_IRR~poly(GeoHHI,2)+poly(StageHHI,2)+ poly(PISHHI,2)+LNumber_Investments+LTotal_Investments+as.factor(Deal_Year)+as.factor(Company_Country)+as.factor(PIS),data = dealdf)

ols <- lm(Gross_IRR~Fund_GeoHHI+Fund_StageHHI+Fund_PISHHI+Number_Investments+Total_Investments+as.factor(Deal_Year),data = dealdf)
output <- huxreg(ols, statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'adj. R squared' = 'adj.r.squared', 'F statistic' = 'statistic', 'P value' = 'p.value'))
print(output)
wb <- as_Workbook(output)
saveWorkbook(wb,"ols.xlsx")
# plot(ols)
#
#
#


# identify outliers
outlier <- outlierTest(ols)
outliers <- as.numeric(names(outlier[["rstudent"]]))

# kill outliers
subdf <- subset(dealdf,!(rownames(dealdf) %in% outliers))

# run analysis again
ols2 <- lm(Gross_IRR~Fund_GeoHHI+Fund_StageHHI+Fund_PISHHI+Number_Investments+Total_Investments+as.factor(Deal_Year),data = subdf)
output2 <- huxreg(ols2, statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'adj. R squared' = 'adj.r.squared', 'F statistic' = 'statistic', 'P value' = 'p.value'))
print(output2)
wb2 <- as_Workbook(output2)
saveWorkbook(wb2,"ols2.xlsx")
outlierTest(ols2)
# plot(ols2)
#
#
#

# run polynomial analysis
ols3 <- lm(Gross_IRR~poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+poly(Fund_PISHHI,2)+Number_Investments+Total_Investments+as.factor(Deal_Year),data = subdf)
output3 <- huxreg(ols3, statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'adj. R squared' = 'adj.r.squared', 'F statistic' = 'statistic', 'P value' = 'p.value'))
print(output3)
wb3 <- as_Workbook(output3)
saveWorkbook(wb3,"ols3.xlsx")
outlierTest(ols3)
#plot(ols3)

# reducing even more outliers
# identify outliers
outlier <- outlierTest(ols3)
outliers <- as.numeric(names(outlier[["rstudent"]]))

# transform to normally distribute residuals

ols4 <- lm(LGross_IRR~poly(LFund_GeoHHI,2)+poly(LFund_StageHHI,2)+poly(LFund_PISHHI,2)+LNumber_Investments+LTotal_Investments+as.factor(Deal_Year),data = subdf)
output4 <- huxreg(ols4, statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'adj. R squared' = 'adj.r.squared', 'F statistic' = 'statistic', 'P value' = 'p.value'))
print(output4)
wb4 <- as_Workbook(output4)
saveWorkbook(wb4,"ols4.xlsx")
outlierTest(ols4)
shapiro.test(ols4$residuals)
#plot(ols4)


# test multicolinearity

#1. correlation table
variables <- cbind(subdf$LFund_GeoHHI, subdf$LFund_StageHHI, subdf$LFund_PIGHHI, subdf$LFund_PISHHI, subdf$LFund_PICHHI, subdf$LNumber_Investments, subdf$LTotal_Investments)
colnames(variables) <- c(fund_hhis,"LNumber_Investments","LTotal_Investments")
M <- cor(variables)
p.mat <- cor.mtest(variables)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)
#2. VIF
vif(ols4)

# improved model
# ols5 <- lm(LGross_IRR~poly(LFund_GeoHHI,2)+poly(LFund_StageHHI,2)+poly(LFund_PISHHI,2)+LNumber_Investments +as.factor(Deal_Year),data = subdf)
# output5 <- huxreg(ols5, statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'adj. R squared' = 'adj.r.squared', 'F statistic' = 'statistic', 'P value' = 'p.value'))
# print(output5)
# wb5 <- as_Workbook(output5)
# saveWorkbook(wb5,"ols5.xlsx")
# outlierTest(ols5)
# shapiro.test(ols5$residuals)
# plot(ols5)


# heteroskedastizity
ncvTest(ols4)
#spreadLevelPlot(ols4)
homodf <- subset(subdf,!(rownames(subdf) %in% c(395)))

ols6 <- lm(LGross_IRR~poly(LFund_GeoHHI,2)+poly(LFund_StageHHI,2)+poly(LFund_PISHHI,2)+LNumber_Investments +as.factor(Deal_Year),data = homodf)
output6 <- huxreg(ols6, statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'adj. R squared' = 'adj.r.squared', 'F statistic' = 'statistic', 'P value' = 'p.value'))
print(output6)
wb6 <- as_Workbook(output6)
saveWorkbook(wb6,"ols6.xlsx")
outlierTest(ols6)
shapiro.test(ols6$residuals)
ncvTest(ols6)
#plot(ols6)

# include more controll variables
ols7 <- lm(LGross_IRR~poly(LFund_GeoHHI,2)+poly(LFund_StageHHI,2)+poly(LFund_PISHHI,2)+LNumber_Investments+Operating_Years+Deal_Size+as.factor(Deal_Year),data = homodf)
output7 <- huxreg(ols7, statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'adj. R squared' = 'adj.r.squared', 'F statistic' = 'statistic', 'P value' = 'p.value'))
print(output7)
wb7 <- as_Workbook(output7)
saveWorkbook(wb7,"ols7.xlsx")
outlierTest(ols7)
shapiro.test(ols7$residuals)
ncvTest(ols7)
#plot(ols7)

# correlation check
#1. correlation table
variables <- cbind(homodf$LFund_GeoHHI, homodf$LFund_StageHHI, homodf$LFund_PIGHHI, homodf$LFund_PISHHI, homodf$LFund_PICHHI, homodf$LNumber_Investments, homodf$LTotal_Investments, homodf$Deal_Size, homodf$Operating_Years)
colnames(variables) <- c(fund_hhis,"LNumber_Investments","LTotal_Investments", "Deal_Size", "Operating_Years")
M <- cor(variables)
p.mat <- cor.mtest(variables)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)
#2. VIF
vif(ols7)

# remove operating years
ols8 <- lm(LGross_IRR~poly(LFund_GeoHHI,2)+poly(LFund_StageHHI,2)+poly(LFund_PISHHI,2)+LNumber_Investments+Deal_Size+as.factor(Deal_Year),data = homodf)
output8 <- huxreg(ols8, statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'adj. R squared' = 'adj.r.squared', 'F statistic' = 'statistic', 'P value' = 'p.value'))
print(output8)
wb8 <- as_Workbook(output8)
saveWorkbook(wb8,"ols8.xlsx")
outlierTest(ols8)
shapiro.test(ols8$residuals)
ncvTest(ols8)
# plot(ols8)

# add FE
ols9 <- lm(LGross_IRR~poly(LFund_GeoHHI,2)+poly(LFund_StageHHI,2)+poly(LFund_PISHHI,2)+LNumber_Investments+Deal_Size+as.factor(Deal_Year)+as.factor(Fund_ID)++as.factor(Mean_PIS),data = homodf)
output9 <- huxreg(ols9, statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'adj. R squared' = 'adj.r.squared', 'F statistic' = 'statistic', 'P value' = 'p.value'))
print(output9)
wb9 <- as_Workbook(output9)
saveWorkbook(wb9,"ols9.xlsx")
outlierTest(ols9)
shapiro.test(ols9$residuals)
ncvTest(ols9)
plot(ols9)




# normal
# FE
felm <- felm(LGross_IRR~poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+ poly(Fund_AvgHHI,2)+LNumber_Investments+LTotal_Investments | Deal_Year+Company_Country+PIS, data = dealdf)
summary(felm)
#plm <- plm(Gross_IRR~poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+ poly(Fund_AvgHHI,2)+LNumber_Investments+LTotal_Investments, data = dealdf, model = 'within',effect = 'twoways', index = c('Fund_ID', 'Deal_Year', 'Company_Country','PIS'))
earlydf<- dealdf[dealdf$Company_Stage == "Early Stage",]
es <- felm(Gross_IRR~poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+ poly(Fund_AvgHHI,2)+LNumber_Investments+LTotal_Investments | Deal_Year+Company_Country+PIS, data = earlydf)
latedf<- dealdf[dealdf$Company_Stage == "Late Stage",]
ls <- felm(Gross_IRR~poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+ poly(Fund_AvgHHI,2)+LNumber_Investments+LTotal_Investments | Deal_Year+Company_Country+PIS, data = latedf)
output <- huxreg("Total" = as, "Late Stage"= ls, "Early Stage" = es, statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'adj. R squared' = 'adj.r.squared', 'F statistic' = 'statistic', 'P value' = 'p.value'))
print(output)

# RE
# as <- lme(Gross_IRR~poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+ poly(Fund_PISHHI,2)+LNumber_Investments+LTotal_Investments, random = Deal_Year|Company_Country|PIS, data = dealdf)
# earlydf<- dealdf[dealdf$Company_Stage == "Early Stage",]
# es <- lme(Gross_IRR~poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+ poly(Fund_PISHHI,2)+LNumber_Investments+LTotal_Investments, random = Deal_Year|Company_Country|PIS, data = earlydf)
# latedf<- dealdf[dealdf$Company_Stage == "Late Stage",]
# ls <- lme(Gross_IRR~poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+ poly(Fund_PISHHI,2)+LNumber_Investments+LTotal_Investments, random = Deal_Year|Company_Country|PIS, data = latedf)
# output <- huxreg("Total" = as, "Late Stage"= ls, "Early Stage" = es, statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'adj. R squared' = 'adj.r.squared', 'F statistic' = 'statistic', 'P value' = 'p.value'))
# print(output)

# # log ind
# las <- felm(Gross_IRR~poly(LFund_GeoHHI,2)+poly(LFund_StageHHI,2)+ poly(LFund_PISHHI,2)+LNumber_Investments+LTotal_Investments | Deal_Year+Company_Country+PIS, data = dealdf)
# earlydf<- dealdf[dealdf$Company_Stage == "Early Stage",]
# les <- felm(Gross_IRR~poly(LFund_GeoHHI,2)+poly(LFund_StageHHI,2)+ poly(LFund_PISHHI,2)+LNumber_Investments+LTotal_Investments | Deal_Year+Company_Country+PIS, data = earlydf)
# latedf<- dealdf[dealdf$Company_Stage == "Late Stage",]
# lls <- felm(Gross_IRR~poly(LFund_GeoHHI,2)+poly(LFund_StageHHI,2)+ poly(LFund_PISHHI,2)+LNumber_Investments+LTotal_Investments | Deal_Year+Company_Country+PIS, data = latedf)

# # !log ind
# as <- felm(Gross_IRR~poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+ poly(Fund_PISHHI,2)+LNumber_Investments+LTotal_Investments | Deal_Year+Company_Country+PIS, data = dealdf)
# earlydf<- dealdf[dealdf$Company_Stage == "Early Stage",]
# es <- felm(Gross_IRR~poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+ poly(Fund_PISHHI,2)+LNumber_Investments+LTotal_Investments | Deal_Year+Company_Country+PIS, data = earlydf)
# latedf<- dealdf[dealdf$Company_Stage == "Late Stage",]
# ls <- felm(Gross_IRR~poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+ poly(Fund_PISHHI,2)+LNumber_Investments+LTotal_Investments | Deal_Year+Company_Country+PIS, data = latedf)
# 
# output <- huxreg("AS"=as, "LS" = ls, "ES" = es,"LAS"=las, "LLS" = lls, "LES" = les, statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'adj. R squared' = 'adj.r.squared', 'F statistic' = 'statistic', 'P value' = 'p.value'))
# print(output)
# 
# 
# # log dep
# las <- felm(LGross_IRR~poly(LFund_GeoHHI,2)+poly(LFund_StageHHI,2)+ poly(LFund_PISHHI,2)+LNumber_Investments+LTotal_Investments | Deal_Year+Company_Country+PIS, data = dealdf)
# earlydf<- dealdf[dealdf$Company_Stage == "Early Stage",]
# les <- felm(LGross_IRR~poly(LFund_GeoHHI,2)+poly(LFund_StageHHI,2)+ poly(LFund_PISHHI,2)+LNumber_Investments+LTotal_Investments | Deal_Year+Company_Country+PIS, data = earlydf)
# latedf<- dealdf[dealdf$Company_Stage == "Late Stage",]
# lls <- felm(LGross_IRR~poly(LFund_GeoHHI,2)+poly(LFund_StageHHI,2)+ poly(LFund_PISHHI,2)+LNumber_Investments+LTotal_Investments | Deal_Year+Company_Country+PIS, data = latedf)
# 
# 
# # !log dep
# as <- felm(Gross_IRR~poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+ poly(Fund_PISHHI,2)+LNumber_Investments+LTotal_Investments | Deal_Year+Company_Country+PIS, data = dealdf)
# earlydf<- dealdf[dealdf$Company_Stage == "Early Stage",]
# es <- felm(Gross_IRR~poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+ poly(Fund_PISHHI,2)+LNumber_Investments+LTotal_Investments | Deal_Year+Company_Country+PIS, data = earlydf)
# latedf<- dealdf[dealdf$Company_Stage == "Late Stage",]
# ls <- felm(Gross_IRR~poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+ poly(Fund_PISHHI,2)+LNumber_Investments+LTotal_Investments | Deal_Year+Company_Country+PIS, data = latedf)
# 
# output <- huxreg("AS"=as, "LS" = ls, "ES" = es,"LAS"=las, "LLS" = lls, "LES" = les, statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'adj. R squared' = 'adj.r.squared', 'F statistic' = 'statistic', 'P value' = 'p.value'))
# print(output)

# IRR
# ols <- lm(LGross_IRR~poly(LFund_GeoHHI,2)+poly(LFund_StageHHI,2)+ poly(LFund_PISHHI,2)+LNumber_Investments+LTotal_Investments+as.factor(Deal_Year),data = dealdf)
# fe <- plm(LGross_IRR~poly(LFund_GeoHHI,2)+poly(LFund_StageHHI,2)+ poly(LFund_PISHHI,2)+LNumber_Investments+LTotal_Investments, data = dealdf, index = c("Deal_Year"), model = "within") #FE
# re <- plm(LGross_IRR~poly(LFund_GeoHHI,2)+poly(LFund_StageHHI,2)+ poly(LFund_PISHHI,2)+LNumber_Investments+LTotal_Investments, data = dealdf, index = c("Deal_Year"), model = "random") #RE
# output <- huxreg("OLS" = ols, "FE" = fe, "RE" = re)
# phtest(fe, re)
# print(output)


#--sd models--------------------------------------------------------------------------------------------------------

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




#--fund level models--------------------------------------------------------------------------------------------------------
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
