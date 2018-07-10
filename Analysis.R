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
# linear.model <- lm(Fund_Return ~ AvgHHI, data = reduceddf)
# # ggplot(funddf, aes(AvgDiversification, Total.Return)) +
# #   geom_point() +
# #   geom_smooth(method='lm')
# summary(linear.model)
# visreg(linear.model, "AvgHHI")





# quadratic model
ggplot(funddf, aes(StageHHI, Fund_Return)) +
  geom_point() +
  #geom_smooth(method = 'lm') +
  geom_smooth(method = 'glm')


model <- lm(funddf$Fund_Return ~ poly(funddf$StageHHI,2))
summary(model)
confint(model, level=0.95)




set.seed(20)
q <- seq(from=0, to=20, by=0.1)
noise <- rnorm(length(q), mean=10, sd=80)
noisy.y <- y + noise
plot(q,noisy.y,col='deepskyblue4',xlab='q',main='Observed data')
lines(q,y,col='firebrick1',lwd=3)
model <- lm(noisy.y ~ poly(q,3))
summary(model)
plot(fitted(model),residuals(model))
predicted.intervals <- predict(model,data.frame(x=q),interval='confidence', level=0.99)
lines(q,predicted.intervals[,1],col='green',lwd=3)
lines(q,predicted.intervals[,2],col='black',lwd=1)
lines(q,predicted.intervals[,3],col='black',lwd=1)
legend("bottomright",c("Observ.","Signal","Predicted"), 
       col=c("deepskyblue4","red","green"), lwd=3)










avgHHI.model <-
  lm(Fund_Return ~ AvgHHI + AvgHHI ^ 2, data = reducedfunddf)
ggplot(reducedfunddf, aes(AvgHHI, Fund_Return)) +
  geom_point() +
  geom_smooth(method = 'nls', formula = Total.Return ~ AvgHHI)
summary(avgHHI.model)


ggplot(reducedfunddf, aes(x = AvgHHI, y = Fund_Return)) +
  xlab("AvgHHI") +
  ylab("Fund_Return") +
  stat_smooth(
    method = 'nls',
    formula = 'y~x + x^2',
    method.args = list(start = c(a = 0.1646, b = 9.5e-8)),
    se = FALSE
  )

m <- lm(reducedfunddf$Fund_Return ~ poly(reducedfunddf$AvgHHI, 2))   # poly() fits the polynomial with 4+1 terms
summary(m)
# ggplot(reducedfunddf,aes(AvgHHI, Fund_Return))+
#   geom_point()+
#   geom_line(aes(y=fitted(m)))

predicted <- data.frame(x = reducedfunddf$Fund_Return, y = fitted(m))

ggplot(reducedfunddf, aes(x = AvgHHI, y = Fund_Return)) +
  ylab("Fund_Return") +
  xlab("AvgHHI") +
  geom_point() +
  # geom_smooth(method = "lm", aes(colour = "Linear")) +
  # geom_smooth(method = "glm", aes(colour = "Exponential"), formula = y ~ x + x^2)
  geom_smooth(method="lm", aes(color="Exp Model"), formula= (y ~ x^2 + x), se=FALSE, linetype = 1) +
  guides(color = guide_legend("Model Type"))

# visreg(quadratic.model, "AvgHHI", trans=exp, ylab="Fund_Return", partial=TRUE)
# ggplot(reducedfunddf,aes(x=AvgHHI, y=Fund_Return)) + geom_smooth()


# qplot(data=funddf[1:10,], aes(x=AvgDiversification, y=Total.Return)) +
#   stat_summary(fun.data=mean_cl_normal) +
#   geom_smooth(method='lm')

#geom_smooth(method='lm',formula=y~x)


# uniqueGroup <- unique(df$Primary_Industry_Group)
# uniqueCode <- unique(df$Primary_Industry_Code)
# uniqueSector <- unique(df$Primary_Industry_Sector)

# qplot(data = funddf, x = funddf$Fund_IRR)
# linear analysis
# scatter.smooth(x=funddf$StageHHI, y=funddf$Fund_IRR, main="geo")

# qplot(data = dealdf, x = dealdf$PISHHI, main="test")

# ggplot(data=groupdf, aes(x=bucket, y=AvgDiversification, group=1)) +
#   geom_line()+
#   geom_point()
