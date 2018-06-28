#import libraries
library(diverse)


hhi <- function(df,variable,HHIName) {
  
  environment(hhi)
  
  #check variables
  if (missing(df))
    stop("Need to specify df.")
  if (missing(variable))
    stop("Need to specify variable.")
  if (missing(HHIName))
    stop("Need to specify HHIName.")
  if(!is.data.frame(df))
    stop("df must be data frame")
  if(!is.character(variable))
    stop("variable must be a string")
  if(!is.character(HHIName))
    stop("HHIName must be a string")
  
  #get number of funds
  maxFundNr = max(df$Investor_fund_ID)
  funds <- 1:maxFundNr
  
  #loop through funds
  for(a in seq(from=1, to=maxFundNr, by=1)) {
    
    #get df with fund only
    subdf <- subset(df, Investor_fund_ID == a)
    
    #sort by date
    out <- subdf[order(as.Date(subdf$Deal_Date)),]
    
    #get all data of selected variable
    data <- out[[variable]]
    
    #get unique data of selected variable
    uniqueData <- unique(data)
    
    #create empty matrix to arrange data
    hhiMatrix <- matrix(0L, nrow = length(uniqueData), ncol = length(data))
    
    #loop through data
    for (i in seq(from=1,to=length(data))) {
      
      #check which unique data, selected data fits
      index <- match(data[i],uniqueData)
      
      if(i > 1) {
        
        #go through the current column
        for (x in seq(from=1, to=length(uniqueData))) {
          
          #copy values from last column
          hhiMatrix[x,i] <- hhiMatrix[x,i-1]
        }  
      }
      
      #increase value in current column for indexed value
      hhiMatrix[index,i] <- hhiMatrix[index,i] + 1
    }

    #calculate HHI for the entire matrix
    hhiReturn <- diversity(hhiMatrix, type='hh', category_row=TRUE)
    hhiReturn <- hhiReturn[order(as.numeric(rownames(hhiReturn))),,drop=FALSE]
    assign('hhiReturn', hhiReturn, pos=.GlobalEnv)

    #manage output 
    names(hhiReturn) <- c(HHIName)
    if (a == 1) {
      output <- cbind(out,hhiReturn)
    } else {
      output <- rbind(output,cbind(out,hhiReturn))
    }
  }
  return(output)
}

hhiTimeSeries <- function(df, variables) {
  
  colClasses = c("Date", "double", "double")
  col.names = c("Date", "Deal_ID", "Investor_fund_ID")
  
  for (i in 1:length(variables)) {
    col.names[i+3] <- variables[i]
    colClasses[i+3] <- "double"
  }
  
  timeSeriesdf <- read.table(text = "",
                             colClasses = colClasses,
                             col.names = col.names)
  

}

