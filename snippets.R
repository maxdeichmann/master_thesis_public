# normal
# FE
# felm <- felm(LGross_IRR~poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+ poly(Fund_AvgHHI,2)+LNumber_Investments+LTotal_Investments | Deal_Year+Company_Country+PIS, data = dealdf)
# summary(felm)
# plm <- plm(Gross_IRR~poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+ poly(Fund_AvgHHI,2)+LNumber_Investments+LTotal_Investments, data = dealdf, model = 'within',effect = 'twoways', index = c('Fund_ID', 'Deal_Year', 'Company_Country','PIS'))
# earlydf<- dealdf[dealdf$Company_Stage == "Early Stage",]
# es <- felm(Gross_IRR~poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+ poly(Fund_AvgHHI,2)+LNumber_Investments+LTotal_Investments | Deal_Year+Company_Country+PIS, data = earlydf)
# latedf<- dealdf[dealdf$Company_Stage == "Late Stage",]
# ls <- felm(Gross_IRR~poly(Fund_GeoHHI,2)+poly(Fund_StageHHI,2)+ poly(Fund_AvgHHI,2)+LNumber_Investments+LTotal_Investments | Deal_Year+Company_Country+PIS, data = latedf)
# output <- huxreg("Total" = as, "Late Stage"= ls, "Early Stage" = es, statistics = c('# observations' = 'nobs', 'R squared' = 'r.squared', 'adj. R squared' = 'adj.r.squared', 'F statistic' = 'statistic', 'P value' = 'p.value'))
# print(output)

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
