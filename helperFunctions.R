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
    hhiReturn <- hhiReturn[order(as.numeric(rownames(hhiReturn))), , drop = FALSE]
    
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
fundData <- function(dealdf, divIndices, fundDivIndices, mscidf) {
  if (!is.data.frame(dealdf))
    stop("df must be data frame")
  if (!is.data.frame(mscidf))
    stop("mscidf must be data frame")
  if (!is.vector(divIndices))
    stop("divIndices must be vector")
  if (!is.vector(fundDivIndices))
    stop("fundDivIndeces must be vector")
  
  # create columns
  col.names = c(
    "Fund_ID",
    "UpSD",
    "DownSD",
    "MSCI",
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
    "double",
    "double",
    "double",
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
    
    # if (subdf$Fund_ID[1] == 1) {
    #   print(subdf)
    #   print("ds")
    #   print(subdf$Deal_Size)
    #   print("weights")
    #   print(weights)
    #   print("-----------------")
    #   print("weightedaverage")
    #   print(weightedAverage)
    #   print("wt.sd")
    #   print(sd)  
    # }
    
    # sd <- sd(out$Gross_IRR, na.rm = TRUE)
    
    # operating years
    st <- year(out$Deal_Date[1])
    en <- year(out$Deal_Date[nrow(out)])
    operating <- en - st
    
    # msci
    submscidf <- mscidf[mscidf$Year == st-1, ]
    msci <- submscidf$Return[[1]]

    # most common country
    factor <- factor(subdf$Company_Country,unique(dealdf$Company_Country))
    countries <- as.numeric(factor)
    index <- as.numeric(names(which.max(table(countries))))
    
    # up
    irr <- subdf$Gross_IRR
    uset <- c()
    for(x in 1:length(irr)) {
      uset <- c(uset, (max((irr[x]*weights[x])-0,0))^2)
    }
    up <- sqrt(1/nrow(subdf)*sum(uset))
    
    
    # down
    dset <- c()
    for(x in 1:length(irr)) {
      dset <- c(dset, (min((irr[x]*weights[x])-0,0))^2)
    }
    
    down <- sqrt(1/nrow(subdf)*sum(dset))

    if(nrow(out) == 1) {
      country <<- out$Company_Country[nrow(out)]
    } else {
      country <<- unique(dealdf$Company_Country)[index]
    }

    # create row
    row = list(
      out$Fund_ID[nrow(out)],
      up,
      down,
      msci,
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
      row[b+15] <- out[[divIndices[b]]][[nrow(out)]]
    }
    
    funddf[nrow(funddf) + 1,] = row
    
  }
  return(funddf)
}

transformBox <- function(x) {
  Box = boxcox((x) ~ 1, lambda = seq(-6,6,0.1))
  Cox = data.frame(Box$x, Box$y)
  Cox2 = Cox[with(Cox, order(-Cox$Box.y)),]
  Cox2[1,]
  lambda = Cox2[1, "Box.x"]
  T_box = ((x) ^ lambda - 1)/lambda
  return(T_box)
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

scatterTrend <- function(dependent, independent, df, highlight = FALSE) {
  outcome <- list()
  mycolours <- c("highlight" = "red", "normal" = "grey50")
  if(highlight == TRUE) {
    df$highlight <- ifelse(df$Deal_ID == 365, "highlight", "normal") 
  }
  for (i in 1:length(independent)) {
    local({
      i <- i
      mycolours <- mycolours
      df <- df
      if(highlight == TRUE) {
        temp = ggplot(data = df, aes(df[[independent[i]]], df[[dependent]], colour = highlight)) + theme_minimal()
      } else {
        temp = ggplot(data = df, aes(df[[independent[i]]], df[[dependent]])) + theme_minimal()
      }
      temp <- temp + geom_point() + geom_smooth(method = "lm", formula = y ~ poly(x, 2, raw = TRUE)) +
        xlab(independent[i]) + ylab(dependent)
      
      if(highlight == TRUE) {
        temp = temp + scale_color_manual("Status", values = mycolours)
      }
      outcome[[i]] <<- temp
    })
  }
  plot <- grid.arrange(grobs = outcome, top="Main Title")
  print(plot)
}

glmAnalysis <- function(dep, ind,ind2, data, autocorrelationTest = T, linearity = T, plot.results = F) {
  
  fn <- as.formula(paste(dep,ind,sep="~"))
  print(fn)
  glm <- glm(fn, family = binomial, data = data)
  
  
  if(autocorrelationTest == T) {
    print(vif(glm))
  }
  
  if(linearity == T) {
    
    print(ind2)
    add <-c()
    for (i in 1:length(ind2)) {
      new <- paste0("log",ind2[i])
      add <- c(add, new)
      data[new] <- log(data[ind2[i]]) * data[ind2[i]]
      
      if(i < 4) {
        new <- paste0("log2",ind2[i])
        add <- c(add, new)
        data[new] <- log(data[ind2[i]])^2 * data[ind2[i]]^2
      }
    }
    
    addTerm <- paste(add,collapse="+")
    fn1 <- update(fn, paste("~ . +",addTerm))
    df<-data[complete.cases(data),]
    glm1 <- glm(fn1, family = binomial, data = df, maxit=50)
    print("--linearity--------------------------------")
    print(summary(glm1))
    print("----------------------------------")
  }
  
  if (plot.results == TRUE) {
    graphs = list()
    for(i in c("Fund_GeoHHI","Fund_StageHHI","Fund_PISHHI")) {
      graphs[[i]] <- visreg(glm, i,type="conditional",scale = "response" , gg = TRUE) + xlim(0, 1) + theme_minimal()
    }
    
    print(grid.arrange(grobs = graphs), top="Main Title")
    
  }
  
  summary(glm)
  return(glm)
}

olsAnalysis <- function(fn, df, filename,div, autocorrelationTest = FALSE, normalityTest = FALSE, plot.analysis = FALSE, 
                        plot.results = FALSE, correlation = FALSE, endogeneity = FALSE, fundLevel = F) {
  
  ols <- lm(fn,data = df)
  
  # print("---------------")
  # print(summary(lm(fn,data = data,weights=1/(1+0.5*x^2))))
  # print("---------------")
  summary(ols)
  #"https://raw.githubusercontent.com/IsidoreBeautrelet/economictheoryblog/master/robust_summary.R"
  robust_se <- as.vector(summary(ols,robust = T)$coefficients[,"Std. Error"])
  out <- paste0(getwd(),"/",filename)
  capture.output(stargazer(ols, se=list(robust_se),
            column.labels=c("robust"),
            omit = c("Deal_Year"),
            omit.labels = c("Deal Year FE?"),
            align=TRUE, type = "html", out=out))
  
  if(normalityTest == TRUE) {
    print(shapiro.test(ols$residuals))
    print(lmtest::bptest(ols))
  }
  
  if(autocorrelationTest == TRUE) {
    # has to be > 0,05
    print(durbinWatsonTest(ols))
  }
  
  if(endogeneity == TRUE) {
    #http://r-statistics.co/Assumptions-of-Linear-Regression.html
    #cor.test(dealdf$Fund_GeoHHI, ols$residuals)
    
    if(fundLevel == T) {
      name <<- c(div, "Number_Investments","Total_Investments", "error")
    } else {
      name <<- c(div, "Number_Investments","Total_Investments","MSCI","Deal_Size", "Deal_Year", "error")  
    }
    
    variable <<- df[name[1]]
    for(n in 1:length(name)-1) {
      if(n>1) {
        variable <<- cbind(variable, df[name[n]])  
      }
    }

    variable <<- cbind(variable, ols$residuals)
    names(variable) <- name
    correlation(variable,name)

  }
  
  if (plot.analysis == TRUE) {
    histo(ols$residuals, "residuals")
    par(mfrow=c(2,2))
    plot(ols)
  }
  
  if (plot.results == TRUE) {
    graphs = list()
    for(i in div) {
      graphs[[i]] <- visreg(ols, i,type="conditional", gg = TRUE) + xlim(0, 1) + theme_minimal()
    }
    
    print(grid.arrange(grobs = graphs), top="Main Title")
    par(mfrow=c(1,1))
    visreg2d(ols, "Fund_GeoHHI", "Fund_PISHHI", plot.type = "persp")
    
    print("DONE")
  }
  
  if (correlation == TRUE) {
    print(vif(ols))
  }

  return(list(ols,robust_se))
}


logisticPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance
  nullDev <- LogModel$null.deviance
  modelN <- length(LogModel$fitted.values)
  R.l <-  1 -  dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2  ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2           ", round(R.n, 3),    "\n")
  return(c(round(R.l, 3),round(R.cs, 3),round(R.n, 3)))
}
