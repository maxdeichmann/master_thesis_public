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

# A string or a vector of strings of nemonic strings referencing to the available diversity measures. 
# The available measures are: "variety", (Shannon) "entropy", "blau","gini-simpson", "simpson", "hill-numbers", 
# "herfindahl-hirschman", "berger-parker", "renyi", (Pielou) "evenness", "rao", "rao-stirling". 
# A list of short mnemonics for each measure: "v", "e", "gs", "s", "td", "hh", "bp", "re", "ev", "r", and "rs". 
# The default for type is "all" which computes all available formulas.


# calculate and append hhi for deals
divers <- function(df, variable, HHIName, type) {

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
  if (!is.character(type))
    stop("give an diversification type")
  
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
    hhiReturn <- diversity(hhiMatrix, type = type, category_row = TRUE)
    
    # calculate (1 - HHI) to have diversification = 1 and specialisation = 0
    for (i in 1:nrow(hhiReturn)) {
      hhiReturn$HHI[i] <- 1 - hhiReturn$HHI[i]
    }
    
    # naming
    hhiReturn <-
      hhiReturn[order(as.numeric(rownames(hhiReturn))), , drop = FALSE]
    
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
fundData <- function(dealdf, divIndices, fundDivIndices) {
  if (!is.data.frame(dealdf))
    stop("df must be data frame")
  if (!is.vector(divIndices))
    stop("divIndices must be vector")
  if (!is.vector(fundDivIndices))
    stop("fundDivIndeces must be vector")
  
  # create columns
  col.names = c(
    "Fund_ID",
    "Operating_Years",
    "LOperating_Years",
    "Fund_IRR",
    "Fund_Deal_Size",
    "Fund_SD",
    "LFund_SD",
    "Number_Investments",
    "LNumber_Investments",
    "Total_Investments",
    "LTotal_Investments",
    "Popular_Country",
    fundDivIndices
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
    "character",
    rep("double", each = length(fundDivIndices))
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
    
    subdf <- na.omit(out)
    
    # weighted average of irr to get fund irr removing na
    weights <- subdf$Deal_Size/sum(subdf$Deal_Size)
    weightedAverage <- wt.mean(subdf$Gross_IRR, weights)
    sd <- wt.sd(subdf$Gross_IRR,weights)
    # sd <- sd(out$Gross_IRR, na.rm = TRUE)
    
    # operating years
    st <- year(out$Deal_Date[1])
    en <- year(out$Deal_Date[nrow(out)])
    operating <- en - st
    
    # most common country
    factor <- factor(subdf$Company_Country,unique(dealdf$Company_Country))
    countries <- as.numeric(factor)
    index <- as.numeric(names(which.max(table(countries))))

    if(nrow(out) == 1) {
      country <<- out$Company_Country[nrow(out)]
    } else {
      country <<- unique(dealdf$Company_Country)[index]
    }
    
    # create row
    row = list(
      out$Fund_ID[nrow(out)],
      operating,
      log(operating),
      weightedAverage,
      median(subdf$Deal_Size),
      sd,
      log(sd),
      nrow(out),
      log(nrow(out)),
      sum(subdf$Deal_Size),
      log(sum(subdf$Deal_Size)),
      country
    )
    
    for (b in 1:length(divIndices)) {
      row[b+12] <- out[[divIndices[b]]][[nrow(out)]]
    }
    
    funddf[nrow(funddf) + 1,] = row
    
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

# crate dataframe on fund level
dataCleaning <- function(dealdf) {
  if (!is.data.frame(dealdf))
    stop("df must be data frame")
  
  # get number of funds
  funds <- unique(dealdf$Fund_ID)
  nrOfFunds = length(funds)
  
  #loop through funds
  for (a in seq(from = 1, to = nrOfFunds, by = 1)) {
    
    # create fund subset
    subdf <- subset(dealdf, Fund_ID == funds[a])
    
    # sort by date
    out <- subdf[order(as.Date(subdf$Deal_Date)),]
    subdf <- na.omit(out)
    
    # missing IRR -> WA deal size
    weights <- subdf$Deal_Size/sum(subdf$Deal_Size)
    weightedAverage <- wt.mean(subdf$Gross_IRR, weights)
    out$Gross_IRR[is.na(out$Gross_IRR)] <- weightedAverage
    
    # missing country -> most often used
    factor <- factor(subdf$Company_Country,unique(dealdf$Company_Country))
    countries <- as.numeric(factor)
    index <- as.numeric(names(which.max(table(countries))))
    
    if(nrow(out) == 1) {
      country <<- out$Company_Country[nrow(out)]
    } else {
      country <<- unique(dealdf$Company_Country)[index]
    }
    out$Company_Country[is.na(out$Company_Country)] <- country
    
    # missing deal size
    out$Deal_Size[is.na(out$Deal_Size)] <- median(subdf$Deal_Size)
    
    # put subdf in dealdf
    if(a == 1) {
      output <- out
    } else {
      output <- rbind(output, out)
    }
    
  }
  return(output)
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

  coe <- coef(model)
  a <- coe[["poly(LFund_GeoHHI, 2)2"]]

  
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

histo <- function(var, name="x-axis") {
  h <- hist(var, breaks = 30, density = 30, xlab = name)
  xfit <- seq(min(var), max(var), length = 40) 
  yfit <- dnorm(xfit, mean = mean(var), sd = sd(var)) 
  yfit <- yfit * diff(h$mids[1:2]) * length(var) 
  lines(xfit, yfit, col = "black", lwd = 2)
  return(h)
}

correlation <- function(df, names) {
  colnames(df) <- c(names)
  M <<- cor(df)
  p.mat <- cor.mtest(df)
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  corrplot(M, method="color", col=col(200),
           type="upper", #order="hclust",
           addCoef.col = "black", # Add coefficient of correlation
           tl.col="black", tl.srt=45, #Text label color and rotation
           # Combine with significance
           p.mat = p.mat, sig.level = 0.01, insig = "blank"
           # hide correlation coefficient on the principal diagonal
  )
}

scatterTrend <- function(dependent, independent, df) {
  outcome <- list()
  
  for (i in 1:length(independent)) {
    local({
      i <- i
      temp = ggplot(data = df, aes(df[[independent[i]]], df[[dependent]])) + theme_minimal() +
        geom_point() + geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE))+
        xlab(independent[i]) + ylab(dependent)
      outcome[[i]] <<- temp
    })
    
  }
  
  plot <- grid.arrange(grobs = outcome, top="Main Title")
  print(plot)
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