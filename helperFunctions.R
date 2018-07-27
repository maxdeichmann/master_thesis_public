#import libraries
library(plyr)
library(ggplot2)
library(jtools)
library(visreg)
library(stargazer)
library(huxtable)
library(cowplot)
library(diverse)
library(ggplot2)
library(gridExtra)
library(data.table)
library(SDMTools)
library(Hmisc)

# calculate and append hhi for deals
hhi <- function(df, variable, HHIName) {

  #check variables
  if (missing(df))
    stop("Need to specify df.")
  if (missing(variable))
    stop("Need to specify variable.")
  if (missing(HHIName))
    stop("Need to specify HHIName.")
  if (!is.data.frame(df))
    stop("df must be data frame")
  if (!is.character(variable))
    stop("variable must be a string")
  if (!is.character(HHIName))
    stop("HHIName must be a string")
  
  # get number of funds
  funds <- unique(df$Fund_ID)
  nrOfFunds = length(funds)
  
  #loop through funds
  for (a in seq(from = 1, to = nrOfFunds, by = 1)) {
    # create fund subset
    subdf <- subset(df, Fund_ID == funds[a])
    
    # sort by date
    out <- subdf[order(as.Date(subdf$Deal_Date)),]
    
    # get column of selected variable
    data <- out[[variable]]
    
    # get unique data of selected variable
    uniqueData <- unique(data)
    
    # create empty matrix to rearrange data
    hhiMatrix <-
      matrix(0L, nrow = length(uniqueData), ncol = length(data))
    
    # loop rows
    for (i in seq(from = 1, to = length(data))) {
      # copy data if row larger than 1
      if (i > 1) {
        # go through the current column
        for (x in seq(from = 1, to = length(uniqueData))) {
          # copy values from last column
          hhiMatrix[x, i] <- hhiMatrix[x, i - 1]
        }
      }
      
      # check which unique data fits selected data
      index <- match(data[i], uniqueData)
      
      # increase value in current column for indexed value
      hhiMatrix[index, i] <- hhiMatrix[index, i] + 1
    }
    
    # calculate HHI for the entire matrix at every investment stage of the fund
    hhiReturn <-
      diversity(hhiMatrix, type = 'hh', category_row = TRUE)
    
    # clculate (1 - HHI) to have diversification = 1 and specialisation = 0
    for (i in 1:nrow(hhiReturn)) {
      hhiReturn$HHI[i] <- 1 - hhiReturn$HHI[i]
    }
    
    # naming
    hhiReturn <-
      hhiReturn[order(as.numeric(rownames(hhiReturn))), , drop = FALSE]
    assign('hhiReturn', hhiReturn, pos = .GlobalEnv)
    
    # manage output
    names(hhiReturn) <- c(HHIName)
    if (a == 1) {
      output <- cbind(out, hhiReturn)
    } else {
      output <- rbind(output, cbind(out, hhiReturn))
    }
  }
  return(output)
}

# crate dataframe on fund level
fundData <- function(dealdf) {
  if (!is.data.frame(dealdf))
    stop("df must be data frame")
  
  # create columns
  col.names = c(
    "Fund_ID",
    "Operating_Years",
    "LOperating_Years",
    "Fund_IRR",
    "Fund_SD",
    "LFund_SD",
    "Number_Investments",
    "LNumber_Investments",
    "Total_Investments",
    "LTotal_Investments",
    "Fund_Return",
    "Fund_GeoHHI",
    "Fund_StageHHI",
    "Fund_PIGHHI",
    "Fund_PICHHI",
    "Fund_PISHHI",
    "Fund_AvgHHI",
    "LFund_GeoHHI",
    "LFund_StageHHI",
    "LFund_PIGHHI",
    "LFund_PICHHI",
    "LFund_PISHHI",
    "LFund_AvgHHI",
    "Mean_PIC",
    "Mean_PIS",
    "Mean_PIG"
    
  )
  colClasses = c(
    "integer",
    "integer",
    "double",
    "double",
    "double",
    "double",
    "double",
    "double",
    "double",
    "double",
    "double",
    "double",
    "double",
    "double",
    "double",
    "double",
    "double",
    "double",
    "double",
    "double",
    "double",
    "double",
    "double",
    "double",
    "double"
  )
  
  funddf <- read.table(text = "",
                       colClasses = colClasses,
                       col.names = col.names)
  
  # get number of funds
  funds <- unique(dealdf$Fund_ID)
  nrOfFunds = length(funds)
  
  #loop through funds
  for (a in seq(from = 1, to = nrOfFunds, by = 1)) {
    # create fund subset
    subdf <- subset(dealdf, Fund_ID == funds[a])
    
    # sort by date
    out <- subdf[order(as.Date(subdf$Deal_Date)),]
    
    # weighted average of irr to get fund irr removing na
    wt <- out$Deal_Size/sum(out$Deal_Size)
    wa <- wt.mean(out$Gross_IRR, wt)
    sd <- wt.sd(out$Gross_IRR,wt)
    # sd <- sd(out$Gross_IRR, na.rm = TRUE)
    
    # operating years
    a <- year(out$Deal_Date[1])
    b <- year(out$Deal_Date[nrow(out)])
    operating <- b - a

    # create row
    funddf[nrow(funddf) + 1,] = list(
      out$Fund_ID[nrow(out)],
      operating,
      log(operating),
      wa,
      sd,
      log(sd),
      nrow(out),
      log(nrow(out)),
      sum(out$Deal_Size),
      log(sum(out$Deal_Size)),
      wa * sum(out$Deal_Size),
      out$GeoHHI[nrow(out)],
      out$StageHHI[nrow(out)],
      out$PIGHHI[nrow(out)],
      out$PICHHI[nrow(out)],
      out$PISHHI[nrow(out)],
      mean(c(out$PIGHHI[nrow(out)],
             out$PICHHI[nrow(out)],
             out$PISHHI[nrow(out)],
             out$GeoHHI[nrow(out)],
             out$StageHHI[nrow(out)])),
      out$LGeoHHI[nrow(out)],
      out$LStageHHI[nrow(out)],
      out$LPIGHHI[nrow(out)],
      out$LPICHHI[nrow(out)],
      out$LPISHHI[nrow(out)],
      mean(c(out$LPIGHHI[nrow(out)],
            out$LPICHHI[nrow(out)],
            out$LPISHHI[nrow(out)],
            out$LGeoHHI[nrow(out)],
            out$LStageHHI[nrow(out)])),
      round(mean(as.numeric(factor(out$PIC,unique(dealdf$PIC)))),0),
      round(mean(as.numeric(factor(out$PIS,unique(dealdf$PIS)))),0),
      round(mean(as.numeric(factor(out$PIG,unique(dealdf$PIG)))),0)
    )
  }
  return(funddf)
}


hhiBuckets <- function(numBuckets, inputdf, hhis, variables) {
  if (missing(numBuckets) | !is.numeric(numBuckets))
    stop("Check function")
  if (missing(inputdf) | !is.data.frame(inputdf))
    stop("Check function")
  if (missing(hhis) | !is.vector(hhis))
    stop("Check function")
  if (missing(variables) | !is.vector(variables))
    stop("Check function")
  groupdf <- data.frame(matrix(NA, nrow = numBuckets + 1, ncol = 0))
  
  for (x in hhis) {
    outcomes <- c()
    for (a in seq(from = 0,
                  to = 1,
                  by = 1 / numBuckets)) {
      irr <- c()
      investment <- c()
      for (b in seq(from = 1,
                    to = nrow(inputdf),
                    by = 1)) {
        interest <- inputdf[[x]][b]
        if ((interest < a + 1 / numBuckets) & (interest >= a)) {
          irr <- c(irr, inputdf[[variables[1]]][b])
          investment <- c(investment, inputdf[[variables[2]]][b])
        }
      }
      if (length(irr) > 0) {
        outcomes = c(outcomes, crossprod(irr, investment)) # weighted.mean ,na.rm = TRUE
      } else {
        outcomes = c(outcomes, 0)
      }
    }
    groupdf[[x]] <- outcomes
  }
  groupdf[["bucket"]] <- c(0:numBuckets)
  return(groupdf)
  
}

sdDistance <- function(hhis, df) {
  if (missing(df) | !is.data.frame(df))
    stop("Check function - df")
  if (missing(hhis) | !is.vector(hhis))
    stop("Check function - hhi")
  
  for (hhi in hhis) {
    mean <- mean(df[[hhi]])
    sd <- sd(df[[hhi]], na.rm = TRUE)
    
    values <- c()
    for (i in 1:nrow(df)) {
      value <- (df[[hhi]][i] - mean) / sd
      values[i] <- value
    }
    name <- paste0("SD_", hhi, collapse = "_")
    df[[name]] <- values
  }
  return(df)
}



plotModel <- function(model,
                      df,
                      dependent,
                      independent,
                      control,
                      toPredict,
                      xRange,
                      xAxis,
                      yAxis,
                      title) {
  if(missing(xAxis)) {
    xAxis <- "x"
  }
  if(missing(yAxis)) {
    yAxis <- "y"
  }
  if(missing(title)) {
    title <- "Title"
  }
  if(missing(xRange)) {
    xRange <- c(0,1)
  }
  print(independent)
  values <- rep("numeric", length(independent))
  legendTitle <- "Independent variables"
  sequence <- seq(xRange[1], xRange[2], length.out = nrow(df))
  newRange <- read.table(text = "",
                         colClasses = values,
                         col.names = independent) #c(independent,control)
  for (element in sequence) {
    newRange[nrow(newRange) + 1,] = rep(element, length(independent))
  }
  newValues <- predict.lm(model, newRange) #, type = "terms", terms = independent
  pred <- make_predictions(model = model, pred = independent[1])
  print(pred)
  
  
  print(plot(newValues))
  print(newValues)
  plot <- ggplot() +
    ggtitle(title) +
    xlab(xAxis) +
    ylab(yAxis) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    xlim(xRange[1], xRange[2]) +
    scale_colour_discrete(legendTitle)

  for (i in 1:length(independent)) {
    loop_input = paste("geom_point(aes(x = df[[independent[",i,"]]], 
                       y = df[[dependent]], colour = independent[",i,"]))")
    plot <- plot + eval(parse(text = loop_input))
  }

  for (i in 1:length(independent)) {
    loop_input <- paste("geom_line(aes(x = newRange[[independent[",i,"]]], 
                        y = newValues[, toPredict[",i,"]], colour = independent[",i,"]))")
    plot <- plot + eval(parse(text = loop_input))
  }
  print(plot)
  return(plot)
}

plotModel1 <- function(model,
                      df,
                      dependent,
                      independent,
                      xRange,
                      xAxis,
                      yAxis,
                      title) {
  if(missing(xAxis)) {
    xAxis <- "x"
  }
  if(missing(yAxis)) {
    yAxis <- "y"
  }
  if(missing(title)) {
    title <- "Title"
  }
  if(missing(xRange)) {
    xRange <- c(0,1)
  }
  print(independent)
  
  coe <- coef(model)
  a <- coe[["poly(LFund_GeoHHI, 2)2"]]
  print(a)
  print(coe)
  print("Hallo")
  
  plot <- ggplot() +
    ggtitle(title) +
    xlab(xAxis) +
    ylab(yAxis) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    xlim(xRange[1], xRange[2])
    # scale_colour_discrete()

  for (i in 1:length(independent)) {
    loop_input = paste("geom_point(aes(x = df[[independent[",i,"]]],
                       y = df[[dependent]], colour = independent[",i,"]))")
    plot <- plot + eval(parse(text = loop_input))
  }
  fun.1 <- function(x) coe[["poly(LFund_GeoHHI, 2)2"]]*x^2 + coe[["poly(LFund_GeoHHI, 2)1"]]*x
  print(fun.1)
  fun.2 <- function(x) -1 * x + 10
  fun.3 <- function(x) 3 * x + 2
  # plot + stat_function(fun = fun.1)
  # plot + stat_function(fun = fun.2)
  plot + stat_function(fun = sin, colour = "red")

  # for (i in 1:length(independent)) {
  #   loop_input <- paste("geom_line(aes(x = newRange[[independent[",i,"]]],
  #                       y = newValues[, toPredict[",i,"]], colour = independent[",i,"]))")
  #   plot <- plot + eval(parse(text = loop_input))
  # }
  print(plot)
  return(plot)
}

# from: http://www.sthda.com/english/wiki/visualize-correlation-matrix-using-correlogram
# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

# from: https://stackoverflow.com/questions/4787332/how-to-remove-outliers-from-a-dataset/4788102#4788102
# remove_outliers <- function(x, na.rm = TRUE, ...) {
#   qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
#   H <- 1.5 * IQR(x, na.rm = na.rm)
#   y <- x
#   y[x < (qnt[1] - H)] <- NA
#   y[x > (qnt[2] + H)] <- NA
#   y
# }

# visualizeModel <- function(model,df,dependentVariables) {
#   
#   models <- list()
#   for (variable in dependentVariables) {
#     
#     print(variable)
#     print(get(variable))
#     new <- effect_plot(model, pred = get(variable), plot.points = TRUE, data = df) + theme_minimal()
#     print(new)
#     models <- list(models, new)
#     
#   }
#   plot_grid(models) 
# }