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
library(foreign)
library(MASS) 
library(rcompanion)
library(sandwich)
library(lmtest)
library(lmtest)

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

hhiIndices <- c("GeoHHI","StageHHI","PISHHI","PIGHHI","PICHHI")

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

# scatterTrend("LGross_IRR",lfundhhiIndices, dealdf)




# 
# a <- ggplot(dealdf, aes(Total_Investments)) + geom_histogram() + theme_minimal()
# b <- ggplot(dealdf, aes(Log_Total_Investments)) + geom_histogram() + theme_minimal()
# plot_grid(a,b)
# 
# a <- ggplot(dealdf, aes(Deal_Size)) + geom_histogram() + theme_minimal()
# b <- ggplot(dealdf, aes(LDeal_Size)) + geom_histogram() + theme_minimal()
# plot_grid(a,b)

#--plotting fund level-----------------------------------------------------------------------------------------------------
# fund level
# a <- ggplot(funddf, aes(Number_Investments)) + geom_histogram() + theme_minimal()
# b <- ggplot() + geom_histogram(data=funddf, aes(LNumber_Investments)) + theme_minimal()
# plot_grid(a,b)

# a <- ggplot() + geom_histogram(data=funddf, aes(Operating_Years)) + theme_minimal()
# b <- ggplot() + geom_histogram(data=funddf, aes(LOperating_Years)) + theme_minimal()
# plot_grid(a,b)

# a <- ggplot() + geom_histogram(data=funddf, aes(Total_Investments)) + theme_minimal()
# b <- ggplot() + geom_histogram(data=funddf, aes(LTotal_Investments)) + theme_minimal()
# plot_grid(a,b)

# a <- ggplot() + geom_histogram(data=funddf, aes(Number_Investments)) + theme_minimal()
# b <- ggplot() + geom_histogram(data=funddf, aes(LNumber_Investments)) + theme_minimal()
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
# dealdf$LGross_IRR <- log(dealdf$Gross_IRR+2)
# independent <- c("Fund_GeoHHI","Fund_StageHHI","Fund_PISHHI","Fund_PIGHHI","Fund_PICHHI")
# dependent <- "LGross_IRR"
# scatterTrend(dependent,independent,dealdf)

# scatterTrend("Fund_SD",fund_hhis, dealdf, highlight = FALSE)
# scatterTrend("LGross_IRR",c("Fund_SD"), dealdf, highlight = FALSE)

#--correlation matrices-----------------------------------------------------------------------------------------------------

# correlation matrices
# ind <- c(dealdf$GeoHHI, dealdf$StageHHI, dealdf$PIGHHI, dealdf$PISHHI, dealdf$PICHHI)
# ma <- matrix(ind, ncol = 5, nrow = nrow(dealdf))
# cor(dealdf$Gross_IRR, ma)



#RETURN ANALYSIS--------------------------------------------------------------------------------------------------------


# run polynomial analysis
ols1 <- olsAnalysis(log(Gross_IRR+2)~poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+poly(Fund_PIGHHI,2)+as.factor(Deal_Year),
            dealdf,
            "IRR_ols1.xlsx",
            normalityTest = TRUE,
            plot = FALSE)

# outlier
subdf <- subset(dealdf,!(rownames(dealdf) %in% c(395))) # deal_ID: 365

ols2 <- olsAnalysis(log(Gross_IRR+2)~poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+poly(Fund_PIGHHI,2)+as.factor(Deal_Year),
            subdf,
            "IRR_ols2.xlsx",
            normalityTest = TRUE,
            plot = FALSE)

# Next step: transform variables: http://rcompanion.org/handbook/I_12.html
# box cox
Box = boxcox((dealdf$Gross_IRR+2) ~ 1, lambda = seq(-6,6,0.1))
Cox = data.frame(Box$x, Box$y)
Cox2 = Cox[with(Cox, order(-Cox$Box.y)),]
Cox2[1,]
lambda = Cox2[1, "Box.x"]
T_box = ((dealdf$Gross_IRR+2) ^ lambda - 1)/lambda
hist(T_box)


# turkey
T_tuk = transformTukey(dealdf$Gross_IRR+2, plotit=FALSE)
hist(T_tuk)

shapiro.test(T_box)
shapiro.test(T_tuk)

ols3 <- olsAnalysis(T_box~poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+poly(Fund_PIGHHI,2)+as.factor(Deal_Year),
                    dealdf,
                    "IRR_ols3.xlsx",
                    normalityTest = TRUE,
                    plot = FALSE)


#Next step:  add independent variables
# check correlation
va <- cbind(dealdf$Fund_GeoHHI, dealdf$Fund_StageHHI, dealdf$Fund_PISHHI, dealdf$Fund_PIGHHI,
            dealdf$LFund_PICHHI, dealdf$Number_Investments, dealdf$Total_Investments, dealdf$Operating_Years,
            dealdf$Deal_Year, dealdf$Deal_Size)
na <- c(fund_hhis,"Number_Investments","Total_Investments", "Operating_Years", "Deal_Year", "Deal_Size")
correlation(va,na)

ols4 <- olsAnalysis(T_box~poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+poly(Fund_PIGHHI,2)+Operating_Years+Deal_Size+
                      as.factor(Deal_Year),
                    dealdf,
                    "IRR_ols4.xlsx",
                    normalityTest = TRUE,
                    plot = FALSE)
# VIF
vif(ols4)


# Next step: Robust errors http://data.princeton.edu/wws509/r/robust.html
co <- coeftest(ols4, vcov = vcovHC(ols4, type="HC1"))
print(co)

#next step: other independent
# ols4 <- olsAnalysis(Success~poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+poly(Fund_PIGHHI,2)+Operating_Years+Deal_Size+
#                       as.factor(Deal_Year),
#                     dealdf,
#                     "IRR_ols4.xlsx",
#                     normalityTest = TRUE,
#                     plot = TRUE)

glm <- glm(Success~poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+poly(Fund_PIGHHI,2)+Operating_Years+Deal_Size+
             as.factor(Deal_Year), family = binomial(), data = dealdf)



# Next step: Robustness
ols5 <- olsAnalysis(T_box~poly(Fund_GeoEI,2)+poly(Fund_StageEI,2)+poly(Fund_PIGEI,2)+Operating_Years+Deal_Size+
                      as.factor(Deal_Year),
                    dealdf,
                    "IRR_ols5.xlsx",
                    normalityTest = TRUE,
                    plot = FALSE)
output <- huxreg(ols4,ols5, statistics = c('# observations' = 'nobs',
                                           'R squared' = 'r.squared',
                                           'adj. R squared' = 'adj.r.squared',
                                           'F statistic' = 'statistic',
                                           'P value' = 'p.value'),
                 error_pos = 'right')
openxlsx::saveWorkbook(as_Workbook(output),"IRR_ols5.xlsx", overwrite = TRUE)

plot_summs(ols4, ols5, model.names = c("HHI","EI"))

# Next step: Robust errors http://data.princeton.edu/wws509/r/robust.html
co <- coeftest(ols5, vcov = vcovHC(ols5, type="HC1"))
print(co)

# END OF RETURN ANALYSIS



#Risk ANALYSIS--------------------------------------------------------------------------------------------------------

# 
# ols1 <- olsAnalysis(Fund_SD~poly(GeoHHI,2)+poly(StageHHI,2)+poly(PIGHHI,2)+Operating_Years+Deal_Size+as.factor(Deal_Year),
#                     dealdf,
#                     "SD_ols1.xlsx",
#                     normalityTest = TRUE,
#                     plot = FALSE)
# 
# # next step: transform dependent variable
# T_tuk = transformTukey(dealdf$Fund_SD+2, plotit=FALSE)
# 
# ols2 <- olsAnalysis(T_tuk~poly(GeoHHI,2)+poly(StageHHI,2)+poly(PIGHHI,2)+Operating_Years+Deal_Size+as.factor(Deal_Year),
#                     dealdf,
#                     "SD_ols2.xlsx",
#                     normalityTest = TRUE,
#                     plot = FALSE)
# 
# # next step: robust errors
# co <- coeftest(ols2, vcov = vcovHC(ols2, type="HC1"))
# print(co)
# 
# # Next step: robustness
# ols3 <- olsAnalysis(T_tuk~poly(GeoEI,2)+poly(StageEI,2)+poly(PIGEI,2)+Operating_Years+Operating_Years+Deal_Size+
#                     as.factor(Deal_Year),
#                     dealdf,
#                     "SD_ols3.xlsx",
#                     normalityTest = TRUE,
#                     plot = FALSE)
# output <- huxreg(ols2,ols3, statistics = c('# observations' = 'nobs', 
#                                            'R squared' = 'r.squared', 
#                                            'adj. R squared' = 'adj.r.squared', 
#                                            'F statistic' = 'statistic', 
#                                            'P value' = 'p.value'), 
#                  error_pos = 'right')
# openxlsx::saveWorkbook(as_Workbook(output),"SD_ols3.xlsx", overwrite = TRUE)
# 
# plot_summs(ols2, ols3, model.names = c("HHI","EI"))
# 
# co <- coeftest(ols3, vcov = vcovHC(ols3, type="HC1"))
# print(co)


#Risk/Return ANALYSIS-------------------------------------------------------------------------------------


# ols1 <- olsAnalysis(Gross_IRR~Fund_SD+Operating_Years+Operating_Years+Deal_Size+as.factor(Deal_Year),
#                     dealdf,
#                     "SDIRR_ols1.xlsx",
#                     normalityTest = TRUE,
#                     plot = FALSE)
# 
# # next step: transform dependent variable
# T_tuk = transformTukey(dealdf$Gross_IRR+2, plotit=FALSE)
# 
# ols2 <- olsAnalysis(T_tuk~Fund_SD+Operating_Years+Operating_Years+Deal_Size,
#                     dealdf,
#                     "SDIRR_ols2.xlsx",
#                     normalityTest = TRUE,
#                     plot = FALSE,
#                     correlation = FALSE)
# 
# 
# co <- coeftest(ols2, vcov = vcovHC(ols2, type="HC1"))
# print(co)
# 
# ols3 <- olsAnalysis(T_tuk~poly(LFund_SD,2)+Operating_Years+Operating_Years+Deal_Size,
#                     dealdf,
#                     "SDIRR_ols2.xlsx",
#                     normalityTest = TRUE,
#                     plot = FALSE,
#                     correlation = TRUE)

#Test-------------------------------------------------------------------------------------
# ols1 <- olsAnalysis(LGross_IRR~LTotal_Investments+as.factor(Deal_Year),
#                     dealdf,
#                     "Test_ols1.xlsx",
#                     normalityTest = TRUE,
#                     plot = TRUE)


