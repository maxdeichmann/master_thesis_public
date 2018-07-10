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

# set wd and load dfs
setwd("/Users/maximiliandeichmann/Development/MasterThesis")
load("dataPreperation_deal.Rda")
load("dataPreperation_fund.Rda")
load("dataPreperation_group.Rda")

funddf <- reducedfunddf

# descriptives
# summary(dealdf)
# summary(reducedfunddf)
# summary(groupdf)

# A <- structure(list(Time = c(0, 1, 2, 4, 6, 8, 9, 10, 11, 12, 13,
#                              14, 15, 16, 18, 19, 20, 21, 22, 24, 25, 26, 27, 28, 29, 30),
#                     Counts = c(126.6, 101.8, 71.6, 101.6, 68.1, 62.9, 45.5, 41.9,
#                                46.3, 34.1, 38.2, 41.7, 24.7, 41.5, 36.6, 19.6,
#                                22.8, 29.6, 23.5, 15.3, 13.4, 26.8, 9.8, 18.8, 25.9, 19.3)), .Names = c("Time", "Counts"),
#                row.names = c(1L, 2L, 3L, 5L, 7L, 9L, 10L, 11L, 12L, 13L, 14L, 15L, 16L, 17L, 19L, 20L, 21L, 22L, 23L, 25L, 26L, 27L, 28L, 29L, 30L, 31L),
#                class = "data.frame")
# attach(A)
# names(A)
#
# linear.model <-lm(Counts ~ Time)
# plot(Time, Counts, pch=16, ylab = "Counts ", cex.lab = 1.3, col = "red" )
# abline(lm(Counts ~ Time), col = "blue")
# Time2 <- Time^2
# quadratic.model <-lm(Counts ~ Time + Time2)
# timevalues <- seq(0, 30, 0.1)
# predictedcounts <- predict(quadratic.model,list(Time=timevalues, Time2=timevalues^2))
# plot(Time, Counts, pch=16, xlab = "Time (s)", ylab = "Counts", cex.lab = 1.3, col = "blue")
# lines(timevalues, predictedcounts, col = "darkgreen", lwd = 3)


# linear model
linear.model <- lm(Fund_Return ~ StageHHI, data = funddf)
summary(linear.model)
# # ggplot(funddf, aes(AvgDiversification, Total.Return)) +
# #   geom_point() +
# #   geom_smooth(method='lm')
# summary(linear.model)
# visreg(linear.model, "AvgHHI")
ggplot() +
  geom_point(aes(x=funddf$StageHHI, y=funddf$Fund_Return), colour = "red") +
  geom_line(aes(x=funddf$StageHHI, y=predict(linear.model, newdata = funddf)), colour = "blue") +
  ggtitle("Linear Regression") +
  xlab("HHI") +
  ylab("Fund Return")

# quadratic model
funddf$StageHHI2 = funddf$StageHHI ^ 2
quadratic.model <- lm(Fund_Return ~ StageHHI + StageHHI2, data = funddf) 
summary(quadratic.model)

ggplot() +
  geom_point(aes(x=funddf$StageHHI, y=funddf$Fund_Return), colour = "red") +
  geom_line(aes(x=funddf$StageHHI, y=predict(quadratic.model)), colour = "blue") +
  ggtitle("Polynomial Regression") +
  xlab("HHI") +
  ylab("Fund Return")