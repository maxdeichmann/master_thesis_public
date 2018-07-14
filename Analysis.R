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

# import sources
setwd("/Users/maximiliandeichmann/Development/MasterThesis")
source("helperFunctions.R")

# set wd and load dfs
setwd("/Users/maximiliandeichmann/Development/MasterThesis")
load("dataPreperation_deal.Rda")
load("dataPreperation_fund.Rda")
load("dataPreperation_group.Rda")
load("controlVector.RData")

# group analysis
# ggplot(groupdf, aes(bucket, GeoHHI)) + geom_point() + theme_minimal() + geom_smooth()
# ggplot(groupdf, aes(bucket, StageHHI)) + geom_point() + theme_minimal() + geom_smooth()
# ggplot(groupdf, aes(bucket, PIGHHI)) + geom_point() + theme_minimal() + geom_smooth()
# ggplot(groupdf, aes(bucket, PICHHI)) + geom_point() + theme_minimal() + geom_smooth()
# ggplot(groupdf, aes(bucket, PISHHI)) + geom_point() + theme_minimal() + geom_smooth()

# log creation
# dealdf$glog <- log(dealdf$GeoHHI)
# dealdf$slog <- log(dealdf$StageHHI)
# dealdf$piglog <- log(dealdf$PIGHHI)
# dealdf$piclog <- log(dealdf$PICHHI)
# dealdf$pislog <- log(dealdf$PISHHI)
# dealdf$lIRR <- log(dealdf$Gross_IRR)
# dealdf$lavg <- log(dealdf$Fund_AvgHHI)
# dealdf$log <- log(dealdf$SDFund_PICHHI)
 
is.na(dealdf) <- sapply(dealdf, is.infinite)
dealdf <- na.omit(dealdf)


# independent variables
# a <- ggplot() + geom_histogram(data=dealdf, aes(GeoHHI)) + theme_minimal()
# b <- ggplot() + geom_histogram(data=dealdf, aes(StageHHI)) + theme_minimal()
# c <- ggplot() + geom_histogram(data=dealdf, aes(PIGHHI)) + theme_minimal()
# d <- ggplot() + geom_histogram(data=dealdf, aes(PICHHI)) + theme_minimal()
# e <- ggplot() + geom_histogram(data=dealdf, aes(PISHHI)) + theme_minimal()
# plot_grid(a,b,c,d,e)
# 
# a <- ggplot() + geom_histogram(data=dealdf, aes(SD_GeoHHI)) + theme_minimal()
# b <- ggplot() + geom_histogram(data=dealdf, aes(SD_StageHHI)) + theme_minimal()
# c <- ggplot() + geom_histogram(data=dealdf, aes(SD_PIGHHI)) + theme_minimal()
# d <- ggplot() + geom_histogram(data=dealdf, aes(SD_PICHHI)) + theme_minimal()
# e <- ggplot() + geom_histogram(data=dealdf, aes(SD_PISHHI)) + theme_minimal()
# plot_grid(a,b,c,d,e)

a <- ggplot() + geom_histogram(data=funddf, aes(Fund_GeoHHI)) + theme_minimal()
b <- ggplot() + geom_histogram(data=funddf, aes(Fund_StageHHI)) + theme_minimal()
c <- ggplot() + geom_histogram(data=funddf, aes(Fund_PIGHHI)) + theme_minimal()
d <- ggplot() + geom_histogram(data=funddf, aes(Fund_PICHHI)) + theme_minimal()
e <- ggplot() + geom_histogram(data=funddf, aes(Fund_PISHHI)) + theme_minimal()
plot_grid(a,b,c,d,e)
  
  
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

# a <- ggplot(dealdf, aes(Number_Investments)) + geom_histogram() + theme_minimal()
# b <- ggplot(dealdf, aes(Log_Number_Investments)) + geom_histogram() + theme_minimal()
# plot_grid(a,b)
# 
# a <- ggplot(dealdf, aes(Number_Investments)) + geom_histogram() + theme_minimal()


# dependent variables
# ggplot(dealdf, aes(Gross_IRR)) + geom_histogram() + theme_minimal()
# ggplot(funddf, aes(Fund_SD)) + geom_histogram() + theme_minimal()

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


# add variables to control vector
controlVector <- c("Number_Investments", controlVector)


# r^2 of 0,1028 @ irr interval @ 0.05 - 0.95
# f1 <- formula(paste("Gross_IRR ~ Fund_GeoHHI + Fund_StageHHI + Fund_PISHHI + ", paste(controlVector, collapse=" + ")))
# r^2 of 12,38 @ irr + deal size interval of 0.01 + 0.9
# f1 <- formula(paste("StageHHI ~ poly(bucket,2)"))
# r^2 of 10; very high acceptance of variables
# f1 <- formula(paste("Fund_SD ~ Fund_PIGHHI + Fund_StageHHI + Fund_GeoHHI +", paste(controlVector, collapse=" + ")))

# f1 <- formula(paste("Fund_SD ~ poly(Fund_PIGHHI,2) + poly(Fund_StageHHI,2) + poly(Fund_GeoHHI,2) +", paste(controlVector, collapse=" + ")))

f1 <- formula(paste("SD_Gross_IRR ~ poly(Fund_PIGHHI,2) + poly(Fund_StageHHI,2) + poly(Fund_GeoHHI,2) +", paste(controlVector, collapse=" + ")))

model1 <- lm(f1,data = dealdf)
print(summary(model1))

effect_plot(model1, pred = Fund_PIGHHI, plot.points = TRUE, data = dealdf, point.size = 0.1)
effect_plot(model1, pred = Fund_StageHHI, plot.points = TRUE, data = dealdf, point.size = 0.1)
effect_plot(model1, pred = Fund_GeoHHI, plot.points = TRUE, data = dealdf, point.size = 0.1)
plot_summs(model1, scale = TRUE)
export_summs(model1)

summary(model1)


#newRange <- expand.grid(subdf)
newRange <- data.frame(Fund_PIGHHI = seq(0, 1, length.out = nrow(dealdf)),Fund_StageHHI = seq(0, 1, length.out = nrow(dealdf)),Fund_GeoHHI = seq(0, 1, length.out = nrow(dealdf)))
newRange$Fund_PIGHHI <- predict(model1, newRange)


plot <- ggplot() +
  theme_minimal() +
  xlim(0, 1) +
  geom_point(aes(x = dealdf$Fund_PIGHHI, y = dealdf$SD_Gross_IRR), colour = "red") +
  geom_point(aes(x = dealdf$Fund_StageHHI, y = dealdf$SD_Gross_IRR), colour = "blue") +
  geom_point(aes(x = dealdf$Fund_GeoHHI, y = dealdf$SD_Gross_IRR)) +
  geom_line(aes(x = newdata$x, y = newRange$Fund_PIGHHI), colour = "red") +
  ggtitle("Regression") +
  xlab("SD_HHI") +
  ylab("SD_IRR")
print(plot)







# for (hhi in independent) {
#   
#   # convert variable vector into string
#   cvvector <- paste(dummyVector, collapse =" + ")
#   allIndependentPrint <- c(gsub('_','',hhi), gsub('_','',paste0("",hhi," - squared")), dummyVector)
#   allVariables <- c(dependent, hhi, dummyVector)
#   
#   # create subdf with alternative variable names
#   subdf <- dealdf[allVariables]
#   colnames(subdf) <- c("y", "x", dummyVector)
#   
#   # linear model
#   linear.model <- lm(y ~ ., data = subdf)
#   # linear.model <- lm(y ~ x + dummyVector, data = subdf)
# 
#   # quadratic model
#   quadratic.model <- lm(y ~ poly(x, 2) + ., data = subdf) 
#   # quadratic.model <- lm(y ~ poly(x, 2), data = subdf) 
#   
#   #newRange <- expand.grid(subdf)
#   newRange <- data.frame(x=seq(0,1,length.out=nrow(subdf)), x2=seq(0,1,length.out=nrow(subdf)))
#   newdata <- data.frame(x=seq(0, 1, .01))
#   #newdata$pred1 <- predict(quadratic.model, newdata)
#   
#   
#   predQ <- predict(quadratic.model)#, newdata = subdf)
#   
#   plot <- ggplot() +
#     theme_minimal() +
#     geom_point(aes(x=subdf$x, y=subdf$y)) +
#     xlim(0, 1) +
#     geom_line(aes(x=newRange$x, y=predict(linear.model)), colour = "blue") +
#     geom_line(aes(x=newdata$x, y=newdata$pred1), colour = "red") +
#     ggtitle("Regression") + 
#     xlab(hhi) +
#     ylab(dependent)
#   print(plot)
#   
#   name <- paste0("",dependent, " - ",hhi,".html")
#   titel <- paste0(dependent, " - ",hhi)
#   titel <- gsub('_','',titel)
#   depvar <- gsub('_','',dependent)
#   indvar <- gsub('_','',hhi)
#   indvar2 <- gsub('_','',paste0("",hhi," - squared"))
#   print(titel)
#   capture.output(stargazer(linear.model,quadratic.model, title=titel, align=TRUE, type="html", digits=1, out=name,
#                            dep.var.labels=c(depvar),
#                            covariate.labels=c(indvar,indvar2,dummyVector,"Constant")))
# }



# fund level analysis
#hhis <- c("GeoHHI","StageHHI","PIGHHI","PICHHI","PISHHI")
#dependent <- "Fund_SD"
#analysis(dependent, hhis, funddf)
