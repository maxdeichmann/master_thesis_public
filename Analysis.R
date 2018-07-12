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
dealdf$glog <- log(dealdf$GeoHHI)
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

dealdf$g2 = dealdf$Fund_GeoHHI^2
dealdf$s2 = dealdf$Fund_StageHHI^2
dealdf$pig2 = dealdf$Fund_PIGHHI^2
dealdf$pic2 = dealdf$Fund_PICHHI^2
dealdf$pis2 = dealdf$Fund_PISHHI^2
dealdf$avg2 = dealdf$Fund_AvgHHI^2

model1 <- lm(Fund_SD ~ Fund_GeoHHI+g2+Fund_StageHHI+s2+Fund_PISHHI+pis2 ,data = dealdf)
summary(model1)

dealdf$lIRR <- log(dealdf$Gross_IRR)
model2 <- lm(Gross_IRR ~ poly(StageHHI,2), data = dealdf)

effect_plot(model2, pred = StageHHI, plot.points = TRUE)
plot_summs(model2, scale = TRUE, plot.distributions = TRUE, inner_ci_level = 0.9)
export_summs(model2)



# deal level analysis
independentVariables <- c("Fund_GeoHHI","Fund_StageHHI","Fund_PIGHHI","Fund_PICHHI","Fund_PISHHI", "Fund_AvgHHI")
dependentVariable <- "Fund_SD"


# convert variable vector into string
independent <- independentVariables[1]
indvector <- paste(controlVector, collapse =" + ")
# allIndependentPrint <- c(gsub('_','',independent), gsub('_','',paste0("",independent," - squared")), controlVector)
allVariables <- c(dependentVariable, independentVariables, controlVector)

# create subdf with alternative variable names
subdf <- dealdf[allVariables]
colnames(subdf) <- c("y", "x", controlVector)

# linear model
f <- paste(dependentVariable, "~", indvector, collapse=" + ")
# linear.model <- lm(f, data = subdf)
linear.model <- lm(y ~ x, data = subdf, x = TRUE)

# quadratic model
quadratic.model <- lm(y ~ poly(x, 2) + ., data = subdf) 
# quadratic.model <- lm(y ~ poly(x, 2), data = subdf) 

#newRange <- expand.grid(subdf)
newRange <- data.frame(x=seq(0,1,length.out=nrow(subdf)), x2=seq(0,1,length.out=nrow(subdf)))
newdata <- data.frame(x=seq(0, 1, .01))
#newdata$pred1 <- predict(quadratic.model, newdata)


predQ <- predict(quadratic.model)#, newdata = subdf)

plot <- ggplot() +
  theme_minimal() +
  geom_point(aes(x=subdf$x, y=subdf$y)) +
  xlim(0, 1) +
  geom_line(aes(x=newRange$x, y=predict(linear.model)), colour = "blue") +
  geom_line(aes(x=newdata$x, y=newdata$pred1), colour = "red") +
  ggtitle("Regression") + 
  xlab(independent) +
  ylab(dependent)
print(plot)

name <- paste0("",dependent, " - ",independent,".html")
titel <- paste0(dependent, " - ",independent)
titel <- gsub('_','',titel)
depvar <- gsub('_','',dependent)
indvar <- gsub('_','',independent)
indvar2 <- gsub('_','',paste0("",independent," - squared"))
print(titel)
capture.output(stargazer(linear.model,quadratic.model, title=titel, align=TRUE, type="html", digits=1, out=name,
                         dep.var.labels=c(depvar),
                         covariate.labels=c(indvar,indvar2,controlVector,"Constant")))


















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
