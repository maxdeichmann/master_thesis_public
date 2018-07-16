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

# calculate and append hhi for deals
hhi <- function(df, variable, HHIName) {
  environment(hhi)
  
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
  funds <- unique(df$Investor_fund_ID)
  nrOfFunds = length(funds)
  
  #loop through funds
  for (a in seq(from = 1, to = nrOfFunds, by = 1)) {
    # create fund subset
    subdf <- subset(df, Investor_fund_ID == funds[a])
    
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
    "Fund_IRR",
    "Fund_SD",
    "Number_Investments",
    "Log_Number_Investments",
    "Total_Investments",
    "Log_Total_Investments",
    "Fund_Return",
    "Fund_GeoHHI",
    "Fund_StageHHI",
    "Fund_PIGHHI",
    "Fund_PICHHI",
    "Fund_PISHHI",
    "Fund_AvgHHI",
    "Fund_Avg_GeoHHI",
    "Fund_Avg_StageHHI",
    "Fund_Avg_PIGHHI",
    "Fund_Avg_PICHHI",
    "Fund_Avg_PISHHI"
    
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
    "double"
  )
  
  funddf <- read.table(text = "",
                       colClasses = colClasses,
                       col.names = col.names)
  
  # get number of funds
  funds <- unique(dealdf$Investor_fund_ID)
  nrOfFunds = length(funds)
  
  #loop through funds
  for (a in seq(from = 1, to = nrOfFunds, by = 1)) {
    # create fund subset
    subdf <- subset(dealdf, Investor_fund_ID == funds[a])
    
    # sort by date
    out <- subdf[order(as.Date(subdf$Deal_Date)),]
    # weighted average of irr to get fund irr removing na
    wa <- weighted.mean(out$Gross_IRR, out$Deal_Size, na.rm = TRUE)
    sd <- sd(out$Gross_IRR, na.rm = TRUE)
    
    # create row
    funddf[nrow(funddf) + 1,] = list(
      out$Investor_fund_ID[nrow(out)],
      wa,
      sd,
      nrow(out),
      log(nrow(out)),
      sum(out$Deal_Size),
      log(wa * sum(out$Deal_Size)),
      wa * sum(out$Deal_Size),
      out$GeoHHI[nrow(out)],
      out$StageHHI[nrow(out)],
      out$PIGHHI[nrow(out)],
      out$PICHHI[nrow(out)],
      out$PISHHI[nrow(out)],
      mean(
        c(
          out$GeoHHI[nrow(out)],
          out$StageHHI[nrow(out)],
          out$PIGHHI[nrow(out)],
          out$PICHHI[nrow(out)],
          out$PISHHI[nrow(out)]
        )
      ),
      mean(out$GeoHHI),
      mean(out$StageHHI),
      mean(out$PIGHHI),
      mean(out$PICHHI),
      mean(out$PISHHI)
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
  values <- rep("numeric", length(independent))
  legendTitle <- "Independent variables"

  sequence <- seq(xRange[1], xRange[2], length.out = nrow(dealdf))
  newRange <- read.table(text = "",
                         colClasses = values,
                         col.names = independent)
  for (element in sequence) {
    newRange[nrow(newRange) + 1,] = rep(element, length(independent))
  }

  newValues <- predict(model, newRange, type = "terms")
  plot <- ggplot() +
    ggtitle(title) +
    xlab(xAxis) +
    ylab(yAxis) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    xlim(xRange[1], xRange[2]) +
    scale_colour_discrete(legendTitle)

  for (i in 1:length(independent)) {
    loop_input = paste("geom_point(aes(x = df[[independent[",i,"]]], y = df[[dependent]], colour = independent[",i,"]))")
    plot <- plot + eval(parse(text = loop_input))
  }

  for (i in 1:length(toPredict)) {
    loop_input <- paste("geom_line(aes(x = newRange[[independent[",i,"]]], y = newValues[, toPredict[",i,"]], colour = independent[",i,"]))")
    plot <- plot + eval(parse(text = loop_input))
  }
  print(plot)
  return(plot)
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