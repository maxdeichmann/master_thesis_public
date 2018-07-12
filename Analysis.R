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

# import sources
setwd("/Users/maximiliandeichmann/Development/MasterThesis")
source("helperFunctions.R")

# set wd and load dfs
setwd("/Users/maximiliandeichmann/Development/MasterThesis")
load("dataPreperation_deal.Rda")
load("dataPreperation_fund.Rda")
load("dataPreperation_group.Rda")
load("controlVector.RData")

# dependent variables
# dealdf$glog <- log(dealdf$GeoHHI)
# ggplot(dealdf, aes(GeoHHI)) + geom_histogram() + theme_minimal()
# ggplot(dealdf, aes(glog)) + geom_histogram() + theme_minimal()
# 
# dealdf$slog <- log(dealdf$StageHHI)
# ggplot(dealdf, aes(StageHHI)) + geom_histogram() + theme_minimal()
# ggplot(dealdf, aes(slog)) + geom_histogram() + theme_minimal()
# 
# dealdf$piglog <- log(dealdf$PIGHHI)
# ggplot(dealdf, aes(PIGHHI)) + geom_histogram() + theme_minimal()
# ggplot(dealdf, aes(piglog)) + geom_histogram() + theme_minimal()
# 
# dealdf$piclog <- log(dealdf$PICHHI)
# ggplot(dealdf, aes(PICHHI)) + geom_histogram() + theme_minimal()
# ggplot(dealdf, aes(piclog)) + geom_histogram() + theme_minimal()
# 
# dealdf$pislog <- log(dealdf$PISHHI)
# ggplot(dealdf, aes(PISHHI)) + geom_histogram() + theme_minimal()
# ggplot(dealdf, aes(pislog)) + geom_histogram() + theme_minimal()

# add variables to control vector
controlVector <- c("Number_Investments", "Total_Investments", controlVector)


dealdf$lIRR <- log(dealdf$Gross_IRR)
# dealdf$lavg <- log(dealdf$Fund_AvgHHI)

is.na(dealdf) <- sapply(dealdf, is.infinite)
dealdf <- na.omit(dealdf)


# r^2 of 0,1028 @ irr interval @ 0.05 - 0.95
# f1 <- formula(paste("Gross_IRR ~ Fund_GeoHHI + Fund_StageHHI + Fund_PISHHI + ", paste(controlVector, collapse=" + ")))

f1 <- formula(paste("lIRR ~ Fund_GeoHHI + Fund_StageHHI + Fund_PISHHI + ", paste(controlVector, collapse=" + ")))

model1 <- lm(f1,data = dealdf)
model2 <- lm(Gross_IRR ~  poly(Fund_AvgHHI,2),data = dealdf)

effect_plot(model1, pred = Fund_GeoHHI, plot.points = TRUE)
effect_plot(model1, pred = Fund_StageHHI, plot.points = TRUE)
effect_plot(model1, pred = Fund_PISHHI, plot.points = TRUE)
plot_summs(model1, scale = TRUE)
export_summs(model1)

summary(model1)








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
