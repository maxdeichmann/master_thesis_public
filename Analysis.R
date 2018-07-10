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

# set wd and load dfs
setwd("/Users/maximiliandeichmann/Development/MasterThesis")
load("dataPreperation_deal.Rda")
load("dataPreperation_fund.Rda")
load("dataPreperation_group.Rda")

funddf <- reducedfunddf


hhis <- c("GeoHHI","StageHHI","PIGHHI","PICHHI","PISHHI")
dependent <- "Fund_Return"

for (hhi in hhis) {
  
  print(hhi)
  
  myvars <- c(dependent, hhi)
  subdf <- funddf[myvars]
  colnames(subdf) <- c("y", "x")
  
  # linear model
  linear.model <- lm(y ~ x, data = subdf)
  print(summary(linear.model))
  
  # quadratic model
  subdf$x2 <- subdf$x^2
  quadratic.model <- lm(y ~ x + x2, data = subdf) 
  print(summary(quadratic.model))
  
  plot <- ggplot() +
    geom_point(aes(x=subdf$x, y=subdf$y)) +
    geom_line(aes(x=subdf$x, y=predict(linear.model, newdata = subdf)), colour = "blue") +
    geom_line(aes(x=subdf$x, y=predict(quadratic.model, newdata = subdf)), colour = "red") +
    ggtitle("Linear Regression") + 
    xlab(hhi) +
    ylab(dependent)
  
  # print(stargazer(linear.model, quadratic.model, title="Results", align=TRUE, type="text"))
  print(plot)
}







