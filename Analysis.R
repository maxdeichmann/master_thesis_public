# sources
# https://www.princeton.edu/~otorres/Panel101R.pdf
# http://karthur.org/2016/fixed-effects-panel-models-in-r.html
# draw line: http://www.stat.columbia.edu/~martin/W2024/R9.pdf
# entire process: http://sia.webpopix.org/polynomialRegression1.html#fitting-a-polynomial-of-degree-0
# jitools: https://cran.r-project.org/web/packages/jtools/vignettes/summ.html
# tables: https://cran.r-project.org/web/packages/stargazer/vignettes/stargazer.pdf
# assumptions: http://r-statistics.co/Assumptions-of-Linear-Regression.html

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
library(estimatr)
library(robustHD)
library(RCurl)
library(ivpack)
library(magrittr)
library(tidyverse)
library(broom)


# import sources
setwd("/Users/maximiliandeichmann/Development/MasterThesis")
source("helperFunctions.R")

# import the function from repository
url_robust <-
  "https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
eval(parse(text = getURL(url_robust, ssl.verifypeer = FALSE)),envir=.GlobalEnv)

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

# filter for at least 6 years of firm experience
# subdealdf <- dealdf[dealdf$Operating_Years >= 6 | dealdf$Number_Investments >= 5,]
# subfunddf <- funddf[funddf$Operating_Years >= 6 | funddf$Number_Investments >= 5,]
dealdf <- dealdf[dealdf$Fund_ID != 54,]

dealdf$WGross_IRR = winsorize(dealdf$Gross_IRR, maxval = 5)

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


# Next step: transform variables: http://rcompanion.org/handbook/I_12.html
# box cox
T_box <- transformBox(dealdf$WGross_IRR+2)
shapiro.test(T_box)

# turkey
# T_tuk = transformTukey(dealdf$Gross_IRR+1, plotit=FALSE)
# ST_tuk = transformTukey(subdealdf$Gross_IRR+2, plotit=FALSE)
# hist(T_tuk)
# shapiro.test(T_tuk)

# ols3 <- olsAnalysis(T_tuk~poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+poly(Fund_PIGHHI,2)+Deal_Year,
#                     dealdf,
#                     "IRR_ols3.html",
#                     normalityTest = TRUE,
#                     plot.analysis = FALSE)


#Next step:  add independent variables
#check correlation
# va <- cbind(dealdf$WGross_IRR,dealdf$Fund_GeoEI, dealdf$Fund_StageEI, dealdf$Fund_PISEI, dealdf$Fund_PIGEI,
#             dealdf$LFund_PICEI, dealdf$Number_Investments, dealdf$Total_Investments, dealdf$Operating_Years,
#             dealdf$Deal_Year, dealdf$Deal_Size, dealdf$MSCI)
# na <- c("IRR",fundeiIndices,"Number_Investments","Total_Investments", "Operating_Years", "Deal_Year", "Deal_Size", "MSCI")
# correlation(va,na)
# 
# 
# ols4 <- olsAnalysis(T_box~poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+poly(Fund_PISHHI,2)+Number_Investments+Total_Investments+
#                     MSCI+Deal_Size+as.factor(Deal_Year),
#                     dealdf,
#                     "IRR_ols4.html",
#                     c("Fund_GeoHHI","Fund_StageHHI","Fund_PISHHI"),
#                     normalityTest = TRUE,
#                     autocorrelationTest = TRUE,
#                     correlation = TRUE,
#                     plot.analysis = TRUE,
#                     plot.results = TRUE,
#                     endogeneity = TRUE)
# 
# test <- olsAnalysis(T_box~poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+poly(Fund_PISHHI,2)+Number_Investments+Total_Investments+
#                       MSCI+Deal_Size+as.factor(Deal_Year)+as.factor(Fund_ID),
#                     dealdf,
#                     "IRR_test.html",
#                     c("Fund_GeoHHI","Fund_StageHHI","Fund_PISHHI"),
#                     normalityTest = TRUE,
#                     autocorrelationTest = TRUE,
#                     correlation = TRUE,
#                     plot.analysis = TRUE,
#                     plot.results = TRUE,
#                     endogeneity = TRUE)
# 
# earlydf <- dealdf[dealdf$Company_Stage == "Early Stage",]
# ET_box <- transformBox(earlydf$WGross_IRR+2)
# ols5 <- olsAnalysis(ET_box~poly(Fund_GeoHHI,2)+poly(Fund_PISHHI,2)+Number_Investments+Total_Investments+
#                       MSCI+Deal_Size+as.factor(Deal_Year),
#                     earlydf,
#                     "IRR_ols5.html",
#                     c("Fund_GeoHHI","Fund_PISHHI"),
#                     normalityTest = TRUE,
#                     autocorrelationTest = TRUE,
#                     correlation = TRUE,
#                     plot.analysis = TRUE,
#                     plot.results = TRUE,
#                     endogeneity = TRUE)
# 
# latedf <- dealdf[dealdf$Company_Stage == "Late Stage",]
# LT_box <- transformBox(latedf$WGross_IRR+2)
# ols6 <- olsAnalysis(LT_box~poly(Fund_GeoHHI,2)+poly(Fund_PISHHI,2)+Number_Investments+Total_Investments+
#                       MSCI+Deal_Size+as.factor(Deal_Year),
#                     latedf,
#                     "IRR_ols6.html",
#                     c("Fund_GeoHHI","Fund_PISHHI"),
#                     normalityTest = TRUE,
#                     autocorrelationTest = TRUE,
#                     correlation = TRUE,
#                     plot.analysis = FALSE,
#                     plot.results = TRUE,
#                     endogeneity = TRUE)
# 
# 
# 

# 
# capture.output(
#   stargazer(ols4[[1]], ols5[[1]], ols6[[1]],
#           title="Return Regression Results",
#           dep.var.labels = c("IRR","IRR","IRR","IRR"),
#           se=list(ols4[[2]], ols5[[2]], ols6[[2]]),
#           column.labels=c("Total", "Early Stage","Late Stage"),
#           omit = c("Deal_Year"),
#           omit.labels = c("Deal Year FE"),
# 
#           align=TRUE, type = "html", out = paste0(getwd(),"/","return.html"))
#   )
# 
# 
# glm1 <- glmAnalysis("Loss",
#                     "poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+poly(Fund_PISHHI,2)+Number_Investments+
#                     Total_Investments+MSCI+Deal_Size",
#                     c("Fund_GeoHHI","Fund_StageHHI","Fund_PISHHI","Number_Investments",
#                       "Total_Investments","MSCI","Deal_Size"),
#                     dealdf,
#                     plot.results = T)
# 
# glm2 <- glmAnalysis("TotalLoss",
#                     "poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+poly(Fund_PISHHI,2)+Number_Investments+
#                     Total_Investments+MSCI+Deal_Size",
#                     c("Fund_GeoHHI","Fund_StageHHI","Fund_PISHHI","Number_Investments",
#                       "Total_Investments","MSCI","Deal_Size"),
#                     dealdf,
#                     plot.results = T)
# 
# glm3 <- glmAnalysis("Success",
#                     "poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+poly(Fund_PISHHI,2)+Number_Investments+
#                     Total_Investments+MSCI+Deal_Size",
#                     c("Fund_GeoHHI","Fund_StageHHI","Fund_PISHHI","Number_Investments",
#                       "Total_Investments","MSCI","Deal_Size"),
#                     dealdf,
#                     plot.results = T)
# 
# glm4 <- glmAnalysis("SP500Success",
#                     "poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+poly(Fund_PISHHI,2)+Number_Investments+
#                     Total_Investments+MSCI+Deal_Size",
#                     c("Fund_GeoHHI","Fund_StageHHI","Fund_PISHHI","Number_Investments",
#                       "Total_Investments","MSCI","Deal_Size"),
#                     dealdf,
#                     plot.results = T)
# 
# 
# 
# r1 <- logisticPseudoR2s(glm1)
# r2 <- logisticPseudoR2s(glm2)
# r3 <- logisticPseudoR2s(glm3)
# r4 <- logisticPseudoR2s(glm4)
# stargazer(glm1, glm2, glm3,glm4,
#           add.lines=list(c("Hosmer and Lemeshow",r1[1],r2[1],r3[1],r4[1]), c("Cox and Snell",r1[2],r2[2],r3[2],r4[2]),
#                          c("Nagelkerke",r1[3],r2[3],r3[3],r4[3])),
#           title="Logistic Regression Results",
#           align=TRUE, type = "html", out = paste0(getwd(),"/","logistic.html")
#           )
# 


#Risk ANALYSIS--------------------------------------------------------------------------------------------------------

funddf <- funddf[funddf$Number_Investments > 1, ]
funddf$WFund_SD <- winsorize(funddf$Fund_SD)
funddf$WUpSD <- winsorize(funddf$UpSD)
funddf$WDownSD <- winsorize(funddf$DownSD)
source("helperFunctions.R")
ols1 <- olsAnalysis(WFund_SD~poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+poly(Fund_PISHHI,2)+Number_Investments+Total_Investments,
                    funddf,
                    "SD_ols1.html",
                    c("Fund_GeoHHI","Fund_StageHHI","Fund_PISHHI"),
                    normalityTest = TRUE,
                    autocorrelationTest = TRUE,
                    correlation = TRUE,
                    plot.analysis = TRUE,
                    plot.results = TRUE,
                    endogeneity = TRUE,
                    fundLevel = TRUE)
ols2 <- olsAnalysis(WUpSD~poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+poly(Fund_PISHHI,2)+Number_Investments+Total_Investments,
                    funddf,
                    "SD_ols1.html",
                    c("Fund_GeoHHI","Fund_StageHHI","Fund_PISHHI"),
                    normalityTest = TRUE,
                    autocorrelationTest = TRUE,
                    correlation = TRUE,
                    plot.analysis = TRUE,
                    plot.results = TRUE,
                    endogeneity = TRUE,
                    fundLevel = TRUE)
ols3 <- olsAnalysis(WDownSD~poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+poly(Fund_PISHHI,2)+Number_Investments+Total_Investments,
                    funddf,
                    "SD_ols1.html",
                    c("Fund_GeoHHI","Fund_StageHHI","Fund_PISHHI"),
                    normalityTest = TRUE,
                    autocorrelationTest = TRUE,
                    correlation = TRUE,
                    plot.analysis = TRUE,
                    plot.results = TRUE,
                    endogeneity = TRUE,
                    fundLevel = TRUE)

# ols2 <- olsAnalysis(WFund_SD~poly(Fund_GeoEI,2)+poly(Fund_StageEI,2)+poly(Fund_PISEI,2)+Number_Investments+Total_Investments,
#                     funddf,
#                     "SD_ols2.html",
#                     c("Fund_GeoEI","Fund_StageEI","Fund_PISEI"),
#                     normalityTest = TRUE,
#                     autocorrelationTest = TRUE,
#                     correlation = TRUE,
#                     plot.analysis = TRUE,
#                     plot.results = TRUE,
#                     endogeneity = TRUE,
#                     fundLevel = TRUE)


capture.output(
  stargazer(ols1[[1]], ols2[[1]], ols3[[1]],
          title="Return Regression Results",
          dep.var.labels = c("Weighted SD","Upside SD","Downside SD"),
          se=list(ols1[[2]], ols2[[2]], ols3[[2]]),
          column.labels=c("Total Risk", "Upside Risk","Downside Risk"),
          omit = c("Deal_Year"),
          omit.labels = c("Deal Year FE"),

          align=TRUE, type = "html", out = paste0(getwd(),"/","upsd.html"))
  )



visreg(ols2[[1]], "Fund_GeoEI",type="conditional")

capture.output(
  stargazer(ols1[[1]],
          title="Risk Regression Results",
          dep.var.labels = c("IRR_SD"),
          se=list(ols1[[2]]),
          column.labels=c("Total"),
          align=TRUE, type = "html", out = paste0(getwd(),"/","sd_robust.html"))
  )