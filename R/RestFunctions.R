
reliabilityTest <- function(object, what, ...) {
  UseMethod("reliabilityTest", object)
}

#' Getting the Psychometric class
#'
#' Makes it simple to do basic psychometrics
#' @param object A Psychometric object
#' @param what what type of analyses: Alpha, Omega or Parallel
#' @param ... gh
#' @return A Reliability object that can be used for analyses
#' @export
reliabilityTest.Psychometric <- function(object, what = "Alpha", ...)
{
  reslist2 <- NULL
  printres <- NULL
  GetAlphaCommands <- function()
  {
    res <- NULL
    for(data in object$ScaleItemFrames )
    {
      print(deparse(substitute(object)))
      n <- paste(names(data), collapse = ",")
      res <- append(res, list(paste("psych::alpha(object$OriginalData[cs(", n, ")], check.keys = T,keys=NULL,",
                                    "cumulative=FALSE, title=NULL, max=10,na.rm = TRUE, ",
                                    "n.iter=1,delete=TRUE,use='pairwise',warnings=TRUE,",
                                    "n.obs=NULL,impute=NULL)", sep = "")))
    }
    return(res)
  }
  GetOmegaCommands <- function()
  {
    res <- NULL
    for(data in object$ScaleItemFrames )
    {
      n <- paste(names(data), collapse = ",")
      res <- append(res, list(paste("psych::omega(object$OriginalData[cs(", n, ")],nfactors=3,fm='minres',n.iter=1,",
                                    "p=.05,poly=FALSE,key=NULL,flip=TRUE,digits=2, title='Omega',",
                                    "sl=TRUE,labels=NULL, plot=TRUE,n.obs=NA,rotate='oblimin',",
                                    "Phi=NULL,option='equal',covar=FALSE)", sep = "")))
    }
    return(res)
    
  }
  GetParallelCommands <- function()
  {
    res <- NULL
    for(data in object$ScaleItemFrames )
    {
      n <- paste(names(data), collapse = ",")
      res <- append(res, list(paste("psych::fa.parallel(object$OriginalData[cs(", n, ")]", sep = "")))
    }
    return(res)
    
  }
  
  if (what == "Alpha")
  {
    resList2 <- lapply(object$ScaleItemFrames, FUN = function(x) {return(psych::alpha(x, check.keys = T))})
    printres <- as.data.frame(lapply(resList2, FUN = function(x) return(x$total$raw_alpha)))
    object$RCommands <- GetAlphaCommands()
    object$Name <- "Alpha"
    
  }
  else if (what == "Omega")
  {
    resList2 <- lapply(object$ScaleItemFrames, FUN = psych::omega, nfactors=3,plot=FALSE)
    printres <- as.data.frame(lapply(resList2, FUN = function(x) return(x$omega_h)))
    object$RCommands <- GetOmegaCommands()
    object$Name <- "Omega"
  }
  else if (what == "Parallel")
  {
    resList2 <- mapply(object$ScaleItemFrames, FUN = 
                         function(x,name) 
                         {
                           print(name)
                           list(psych::fa.parallel(x, main = paste("Analysis for ", name), plot = F))},
                       names(object$ScaleItemFrames))
    
    printres <- as.data.frame(lapply(resList2, FUN = function(x,y) return(x$nfact)))
    object$RCommands <- GetParallelCommands()
    object$Name <- "Parallel"
    
    
  }
  else
    return(print("There was no such function"))
  
  
  names(resList2) <- object$ScaleNames
  names(printres) <- object$ScaleNames
  rownames(printres) <- what
  object$ResultList <- resList2
  object$PrintRes <- printres
  
  names(object$RCommands) <- object$ScaleNames
  class(object) <- c( "Reliability", "Psychometric")
  return(object)
}


#' Summary of Reliability
#'
#' Makes it simple to do basic psychometrics
#' @param object A Reliability object
#' @param scale which scale
#' @param ... which scale
#' @return A Reliability object that can be used for analyses
#' @export
summary.Reliability <- function(object, scale = NULL,...)
{
  if (is.null(scale))
    print(object$ResultList)
  else
  {
    if (scale %in% names(object$ScaleFrame))
      print(object$ResultList[scale])
  }
}

#' Summary of print
#'
#' Makes it simple to do basic psychometrics
#' @param x A Reliability object
#' @param plot which scale
#' @param ... Other arguments
#' @return A Reliability object that can be used for analyses
#' @export
print.Reliability <- function(x, plot = F,...)
{
  print(object$PrintRes)
}

#' Summary of plot
#'
#' Makes it simple to do basic psychometrics
#' @param x A Reliability object
#' @param scale which scale
#' @param ... Other arguments
#' @return A Reliability object that can be used for analyses
#' @export
plot.Reliability <- function(x,  scale = NULL,...)
{
  if (class(object)[1] == "Reliability")
  {
    if (object$Name == "Omega")
    {
      if (is.null(scale))
      {
        lapply(object$ScaleItemFrames, FUN = psych::omega, nfactors=3,plot=T)
      }
      else if (!is.null(object$ScaleItemFrames[[scale]]))
        omega.diagram(object$ResultList[[scale]])
      #      omega(object$ScaleItemFrames[[scale]], nfactors = 3, plot = T)
    }
    else if (object$Name == "Parallel")
    {
      if (is.null(scale))
      {
        for (frame in object$ScaleItemFrames)
        {
          print(psych::fa.parallel(frame, main = paste("Analysis for ", names(frame)), plot = T))
        }
      }
      else 
        psych::fa.parallel(object$ScaleItemFrames[[scale]], main = paste("Analysis for ", scale), plot = T)
    }
    
  }
}

imputeMissing <- function(object, ...) {
  UseMethod("imputeMissing", object)
}

#' Summary of imputeMissing
#'
#' Makes it simple to do basic psychometrics
#' @param object A Reliability object
#' @param handleMissing A Reliability object
#' @param scales A Reliability object
#' @param ... which scale
#' @return A Reliability object that can be used for analyses
#' @export
imputeMissing.Psychometric <- function(object, handleMissing = "Listwise", scales = F,...)
{
  HandleMissing <- function(dataToHandle)
  {
    if (handleMissing == "Listwise")
    {
      
      return(dataToHandle[complete.cases(dataToHandle),])
      
    }
    if (handleMissing == "Impute")
    {
      imputed <- mice::mice(dataToHandle, m = 1, method = "norm.predict", printFlag = F)
      return(mice::complete(imputed))
      
    }    
    if (handleMissing == "Mean")
    {
      imputed <- mice(dataToHandle, m = 1, method = "mean", printFlag = F)
      return(complete(imputed))
      
    }
    if (handleMissing == "Bayesian")
    {
      imputed <- mice::mice(dataToHandle, m = 1, method = "norm", printFlag = F)
      return(mice::complete(imputed))
      
    }
    if (handleMissing == "Check")
    {
      print(mice::md.pattern(dataToHandle, plot = TRUE))
      
      return(dataToHandle)
    }
  }
  GetScalesFrame <- function(frames, nameV)
  {
    res <- NULL
    for (index in 1:length(frames))
    {
      res <- cbind(res, rowMeans(as.data.frame(frames[index]), na.rm = F))
    }
    res <- as.data.frame(res)
    row.names(res) <- 1:nrow(res)
    names(res) <- nameV
    return(res)
    
    
  }
  if (scales == T)
  {
    object$ScaleFrame <- HandleMissing(object$ScaleFrame)
  }
  else {
    for(index in 1:length(object$ScaleItemFrames))
    {
      object$ScaleItemFrames[[index]] <- HandleMissing(object$ScaleItemFrames[[index]])
    }
    object$ScaleFrame <- GetScalesFrame(object$ScaleItemFrames, object$ScaleNames)
  }
  
  # Here we change the object to a Missing.Psychometric object. But remember
  # that it can still be used as a Psychometric object
  class(object) <- c("PsychometricMissing", "Psychometric")
  return(object)
}

getCommand <- function(x, ...) {
  UseMethod("getCommand", x)
}

#' Summary of getCommandMissing
#'
#' Makes it simple to do basic psychometrics
#' @param object A Reliability object
#' @param scale A Reliability object
#' @param command Alpha, Omega 
#' @param ... which scale
#' @return A Reliability object that can be used for analyses
#' @export
getCommand.Psychometric <- function(object, scale = "All", command = "Alpha")
{
  getAlpha <- function(scale)
  {
    n <- paste(names(object$ScaleItemFrames[[scale]]), collapse = ",")
    res <- list(paste("psych::alpha(object$OriginalData[cs(", n, ")], check.keys = T,keys=NULL,",
                      "cumulative=FALSE, title=NULL, max=10,na.rm = TRUE, ",
                      "n.iter=1,delete=TRUE,use='pairwise',warnings=TRUE,",
                      "n.obs=NULL,impute=NULL)", sep = ""))
    return(res)
    
  }
  
  if (command == "Alpha" && scale == "All")
  {
    res <- NULL
    
    for(data in object$ScaleItemFrames )
    {
      res <- append(res, getAlpha(scale))
    }
    return(res)
  }
  if (command == "Alpha" && scale != "All")
  {
    if (any(scale == object$ScaleNames))
    {
      return(getAlpha(scale))
    }
    
  }
  
}

' Summary of getCommandMissing
#'
#' Makes it simple to do basic psychometrics
#' @param object A Reliability object
#' @param scale  All, Alpha, Omega 
#' @param ... which scale
#' @return A Reliability object that can be used for analyses
#' @export
getCommand.Reliability <- function(object, scale = "All")
{
  print("Exchange 'object' with your owb object name")
  if (scale == "All")
    return(object$RCommands)
  else if (scale %in% object$ScaleNames)
    return(object$RCommands[scale])
  
}



#' Summary of split
#'
#' Makes it simple to do basic psychometrics
#' @param object A Reliability object
#' @param group  All, Alpha, Omega 
#' @param f  function 
#' @param ... which scale
#' @return A Reliability object that can be used for analyses
#' @export
split.Psychometric <- function(object, group, f,...)
{
  splitDataFrames <- split(object$OriginalData, object$OriginalData[c(group)])
  results <- list()
  lNames <- c()
  for (data in splitDataFrames)
  {
    print(paste("Here are results for group variable ", group,
                "category", data[1, group]))
    Psychometric <- GetPsychometric(data, object$ScaleNames,
                                    responseScale = object$ResponseScales,
                                    itemLength = object$ItemLength, 
                                    reverse = F, name = paste(group, data[1, group]))
    res <- f(Psychometric, ...)
    lNames <- c(lNames, paste(group, as.character(data[group][1,1]), sep = ""))
    results <- append(results, list(res))
  }
  names(results) <- lNames
  return(results)
  
}


getObject <- function(x, ...) {
  UseMethod("getObject", x)
}


#' Summary of getObject
#'
#' Makes it simple to do basic psychometrics
#' @param object A Reliability object
#' @return A Reliability object that can be used for analyses
#' @export
getObject.Psychometric <- function(object)
{
  
  return(object)
}

bestItems <- function(x, ...) {
  UseMethod("bestItems", x)
}

bestItems.Psychometric <- function(object, scale, nItems)
{
  
  psych::bestScales(cbind(object$ScaleItemFrames[[scale]], object$ScaleFrame[scale]),
                    criteria = scale, n.item = nItems-1, dictionary = object$ItemDictionary)
}

plotScale <- function(x, ...) {
  UseMethod("plotScale", x)
}

print.PsychometricList <- function(object)
{
  for(obj in object)
    print(obj)
}

#' Summary of plotScale.Psychometric
#'
#' Makes it simple to do basic psychometrics
#' @param object A Reliability object
#' @param scale A Reliability object
#' @param external  All, Alpha, Omega 
#' @param group  All, Alpha, Omega 
#' @param type  function 
#' @param main  function 
#' @param xlab  function 
#' @param ... which scale
#' @return A Reliability object that can be used for analyses
#' @export
plotScale.Psychometric <- function(object, scale = "All", group = NULL,
                                   external = NULL,
                                   type = "Histogram", main = "", xlab = "", ...)
{
  if(is.null(group) && is.null(external))
  {
    if (scale %in% object$ScaleNames)
    {
      if (type == "Histogram" || missing(type))
      {
        hist(object$ScaleFrame[[scale]], 
             main = ifelse(missing(main),
                           paste("Histogram of", scale, object$Name),
                           main),
             xlab = ifelse(missing(xlab), scale, xlab))
      }
      if (type == "Boxplot")
      {
        #        boxplot(object$ScaleFrame[[scale]], 
        #                xlab = ifelse(missing(xlab), scale, xlab))
        ggplot2::ggplot(data = object$ScaleFrame[scale], aes_string(y = scale))+
          ggplot2::geom_boxplot() + 
          ggplot2::ggtitle(paste("Distribution of ", object$Name))
      }
    }
    else
      print("Wrong scale name")
  }
  else
  {
    if (scale != "All" && is.null(external))
    {
      if(group %in% names(object$OtherVariables))
        
        if (type == "Boxplot")
        {
          f <- formula(paste(scale, "~", group))
          d <- cbind(object$ScaleFrame[scale], factor(object$OtherVariables[group]))
          
          
          boxplot(f, data = d)
        }
    }
    else
    {
      if (!is.null(external))
      {
        if (type == "Scatter" && scale %in% names(object$ScaleFrame) && 
            external %in% names(object$OtherVariables))
        {
          d <- cbind(object$ScaleFrame[scale], object$OtherVariables[external])
          ggplot2::ggplot(data = d, aes_string(x = scale, y = external)) +
            ggplot2::geom_point(col = "gray") + 
            ggplot2::ggtitle(paste("Distribution of ", object$Name))
        }
        
      }
    }
    
  }
  
  
}


plot.Psychometric <- function(object, scale = "All")
{
  if (scale == "All")
  {
    
    for(data in object$ScaleItemFrames )
    {
      corr <- cor(data, use = "pairwise.complete.obs");
      
      print(ggcorrplot::ggcorrplot(corr, hc.order = TRUE, type = "lower",
                                   lab = TRUE))
    }
  }
  if (scale != "All")
  {
    if (any(scale == object$ScaleNames))
    {
      data <- object$ScaleItemFrames[[scale]]
      corr <- cor(data, use = "pairwise.complete.obs")
      ggcorrplot::ggcorrplot(corr, hc.order = TRUE, type = "lower",
                             lab = TRUE)
    }
    
  }
  
}


#Summary table of mean, sd, n, omega, alpha, skew, kurtosis and range

summary.Psychometric<-function(x, mean = T, sd = T, SE = T, skew = T, kurtosis = T,
                               min = T, max = T, omega = T,
                               #alpha = T, 
                               n = T, plots = F) 
{
  y <- x$ScaleFrame
  sumx <- data.frame(Tillf = c(1:ncol(y)))
  for (i in 1:ncol(y))
  {
    if(mean==TRUE)
    {sumx$Mean[i]<-mean(as.numeric(y[,i]), na.rm = TRUE)}
    if(sd==TRUE)
    {sumx$SD[i]<-sd(as.numeric(y[,i]), na.rm = TRUE)}
    if(SE==TRUE) 
    {sumx$SE[i]<-sd(y[,i])/sqrt(sum(!is.na(y[,i])))} # need library(plotrix))
    if(skew==TRUE)
    {sumx$Skew[i]<-skew(as.numeric(y[,i]), na.rm = TRUE)}
    if(kurtosis==TRUE)
    {sumx$Kurtosis[i]<-kurtosi(as.numeric(y[,i]), na.rm = TRUE)}
    if(min==TRUE)
    {sumx$Min[i]<-min(as.numeric(y[,i]), na.rm = TRUE)}
    if(max==TRUE)
    {sumx$Max[i]<-max(as.numeric(y[,i]), na.rm = TRUE)}
    if(omega==TRUE)
    {omeg<-psych::omega(x$ScaleItemFrames[[i]], plot = plots) 
    sumx$Omega[i]<-as.vector(omeg$omega.tot)}
    if(n==TRUE)
    {sumx$N[i]<-length(y[,i][!is.na(y[,i])]) }
  }
  sumx <- sumx[-1]
  summaryy<-sumx
  rownames(summaryy)<-x$ScaleNames
  summaryy<-round(summaryy,3) #round amount of decimals
  if (kurtosis==TRUE)
  {
    if (nrow(y)<300)
    {
      mystars <- ifelse(summaryy$Kurtosis > 2, "*", "") # adding stars to kurtosis and skew value above a certain number
      summaryy$Kurtosis<-paste(summaryy$Kurtosis, mystars, sep="")
      p <- ifelse(summaryy$Kurtosis > 2, NA, summaryy$Kurtosis )
      if(any(is.na(p))) warning('You have scales with high kurtosis, see which values end with "*"')
    }
    else
    {
      mystars <- ifelse(summaryy$Kurtosis > 4, "*", "") # adding stars to kurtosis and skew value above a certain number
      summaryy$Kurtosis<-paste(summaryy$Kurtosis, mystars, sep="")
      p <- ifelse(summaryy$Kurtosis > 4, NA, summaryy$Kurtosis )
      if(any(is.na(p))) warning('You have scales with high kurtosis, see which values end with "*"')
    }
    
  }
  if (skew==TRUE)
  {
    if (nrow(y)<300)
    {
      mystars2 <- ifelse(summaryy$Skew > 1, "*", "")
      summaryy$Skew<-paste(summaryy$Skew, mystars2, sep=" ")
      k <- ifelse(summaryy$skew > 1, NA, summaryy$skew )
      if(any(is.na(k))) warning('You have scales with high skew, see which values end with "*"')
    }
    else
      mystars2 <- ifelse(summaryy$Skew > 2, "*", "")
    summaryy$Skew<-paste(summaryy$Skew, mystars2, sep=" ")
    k <- ifelse(summaryy$skew > 2, NA, summaryy$skew )
    if(any(is.na(k))) warning('You have scales with high skew, see which values end with "*"')
    
  }
  if(omega==TRUE)
  {
    mystars3 <- ifelse(summaryy$Omega < 0.75, "*", "")
    summaryy$Omega<-paste(summaryy$Omega, mystars3, sep=" ")
    O <- ifelse(summaryy$Omega > 0.75, NA, summaryy$Omega )
    if(any(is.na(O))) warning('You have scales with poor reliability, see which values end with "*"')
  }
  return(summaryy)
}

filter.Psychometric<-function(object, keep)
{
  if (nrow(object$ScaleFrame) != length(keep))
  {
    print("Logical vector not the same length as frames")
    return()
  }
  object$ScaleFrame <- dplyr::filter(object$ScaleFrame, keep)
  object$OtherVariables <- dplyr::filter(object$OtherVariables, keep)
  object$OriginalData <- dplyr::filter(object$OriginalData, keep)
  for(index in 1:length(object$ScaleItemFrames))
    object$ScaleItemFrames[[index]] <-
    dplyr::filter(object$ScaleItemFrames[[index]], keep)
  return(object)
}



handleOutliers <- function(x, ...) {
  UseMethod("handleOutliers", x)
}

handleOutliers.Psychometric <- function(object, method = "Mahalanobis", limit = .001,
                                        missing = "None")
{
  getInsideRange <- function(s, r)
  {
    return (ifelse(s >= r[1] & s <= r[2], s, ifelse(s < r[1], r[1], r[2])))
  }
  deleteOutsideRange <- function( s, r)
  {
    return(ifelse (s < r[1], NA, ifelse(s > r[2], NA, s) ))
  }
  if (method == "Mahalanobis") {
    if (missing != "None")
      noMissObject <- imputeMissing(object, handleMissing = missing)
    else
      noMissObject <- object
    scaleCor <- cov(noMissObject$ScaleFrame)
    Outliers <- mahalanobis(noMissObject$ScaleFrame, colMeans(noMissObject$ScaleFrame), scaleCor)
    object <- dplyr::filter(noMissObject, Outliers < qchisq(1-limit, length(object$ScaleNames)))
    return(object)
  }
  if (method == "SD")
  {
    if (missing != "None")
      noMissObject <- imputeMissing(object, handleMissing = missing)
    else
      noMissObject <- object
    newFrame <- NULL
    for(scale in noMissObject$ScaleFrame)
    {
      m <- mean(scale)
      sd <- sd(scale) * qnorm(1 - limit)
      r <- range(m+sd, m-sd)
      newFrame <- cbind(newFrame, deleteOutsideRange(scale, r))
    }
    noMissObject$ScaleFrame <- newFrame
    
    return(imputeMissing(noMissObject, scales = T))
    
  }
  if (method == "Winsorizing" || method == "Change")
  {
    if (missing != "None")
      noMissObject <- imputeMissing(object, handleMissing = missing)
    else
      noMissObject <- object
    newFrame <- NULL
    for(scale in noMissObject$ScaleFrame)
    {
      m <- mean(scale)
      sd <- sd(scale) * qnorm(1 - limit)
      r <- range(m+sd, m-sd)
      newFrame <- cbind(newFrame, getInsideRange(scale, r))
    }
    noMissObject$ScaleFrame <- newFrame
    return(noMissObject)
  }
}

names.Psychometric <- function(object)
{
  print("Scales")
  print(names(object$ScaleFrame))
  print("OtherVariables")
  print(names(object$OtherVariables))
  return()
}

getData <- function(x) {
  UseMethod("getData", x)
}

getData.Psychometric <- function(object)
{
  itemFrames <- NULL
  for(frame in object$ScaleItemFrames)
  {
    if (is.null(itemFrames))
       itemFrames <- frame
    else
       itemFrames <- cbind(itemFrames, frame)
  }
  return(as.data.frame(cbind( object$OtherVariables, object$ScaleFrame,itemFrames)))
}

pHelp <- function(x, ...) {
  UseMethod("pHelp", x)
}
pHelp.Psychometric <- function(object, func = "all")
{
  if (func == "all")
  {
    cat("testReliability(object)\n")
    cat("plot(object)\n")
    cat("summary(object, mean = T, sd = T, SE = T, skew = T, kurtosis = T,
  min = T, max = T, omega = T,n = T, plots = F)\n")
    cat("impute(object)\n")
    cat('handleOutliers(object, method = "Mahalanobis"/"SD"/"Change", limit = .001, missing = "None")\n')
    cat('names(object)\n')
    cat('split(object, group, f,...)\n')
    cat('plotScale(object, scale = "All", group = NULL, external = NULL, type = "Histogram"/"Boxplot", main = "", xlab = "", ...)')
  }
  if (func == "testReliabilitet")
  {
    print("This method/function creates a new class called Reliability, it is an extension of Psychometric")
    print('This is the call: testReliability(object, what = "Alpha"/"Omega"/"Parallel")')
    print('Methods for Reliability is: summary(), getCommand(), plot(), print()')
    print(paste("The method/function creates basic Psychometric anlaysis with Cronbach's alpha,",
                "and omega, and can also perform parallel analysis on the number of" ,
                "components and factors to extract"))
  }
  if (func == "split")
  {
    cat("This method/function splits the dataframes according to the catagories
 of the group variable and performs a function on all
 subframes. The 'group' variable must part of the the object in the 'OtherVariables'
 dataframe. f is a function argument and cann be any of the methods of the 
 Psychometric class. You can add arguments to the function in the end of the
 call\n")
    cat("The call is: split(object, group, f, ...")
  }
  
}
pHelp.Reliability <- function(object, func = "All")
{
  if (func == "All")
  {
    print('summary(object)')
    print('getCommands(object)')
    print('print(object)')
    print('plot(object, scale')
  }
}


CreateItemNames <- function(data, scales, itemList, reverseList=c(), itemLength = 4)
{
  index <- 1
  for (scale in scales)
  {
    for (item in itemList[[index]])
    {
      if (!is.character(item))
      {
        if (item %in% reverseList)
          names(data)[item] <- paste(substr(scale, 1, itemLength), 
                                     names(data[item]), "R", sep = "")
        else
          names(data)[item] <- paste(substr(scale, 1, itemLength), 
                                     names(data[item]), sep = "")
        
      }
      else
      {
        if (item %in% reverseList)
        {
          cn <- which( colnames(data)==item )
          names(data)[cn] <- paste(substr(scale, 1, itemLength),
                                   item, "R", sep = "")
        }
        else
        {
          cn <- which( colnames(data)==item )
          
          names(data)[cn] <- paste(substr(scale, 1, itemLength),
                                   item, sep = "")
        }
        
      }   
    }
    index <- index + 1
    
  }
  return(data)
}

CheckForPsychometric <- function(data, scaleNames, responsScale = list(c(1,5)),
                                 typeSum = "Mean", itemLength = 6,
                                 reverse = T, idVar = "ID", name = "Psychometric",
                                 itemDictionary = NULL)
{
  
  CheckResponsScale <- function(p)
  {
    index <- 1
    resRange <- NULL
    outsideFlag <- F
    for (scales in p)
    {
      if (length(responsScale) > 1 && length(responsScale) >= index)
      {
        rs <- responsScale[[index]]
        index <- index + 1
      }
      else if (length(responsScale) < index)
      {  print(paste("Warning: the number of responsscales are too few", 
                     names(scales), sep = " "))
      }
      else
      {
        rs <- responsScale[[index]] 
        
      }  
      srs <- scales$SuggestedResponsScale
      if (srs[1] < rs[1] || srs[2] >rs[2])
      {
        print(paste("Warning: responseScale outside range for scale", 
                    scales$name, " item ",  sep = " "))
        outsideFlag <- T
      }
    }
    if (outsideFlag == F)
      print("All responses were insde the response scale range")
    
  }
  
  PrintScaleItems <- function(p)
  {
    for (scale in p)
    {
      print(paste("Items for scale: ", scale$name, sep = ""))
      print(toString(scale$items, sep = ","))
      print(paste("Reversed items for scale: ", scale$name, sep = ""))
      print(toString(scale$Reversed, sep = ","))
    }
  }
  
  output <- NULL
  for(index in 1:length(scaleNames))
  {
    rev <- NULL
    fr <- dplyr::select(data, dplyr::starts_with(substr(scaleNames[index], 1, itemLength)))
    if (reverse == T)
    {
      rev <- dplyr::select(fr, dplyr::ends_with("R"))
      
    }
    resList <- unlist(sapply(fr, FUN = function(x)
    {
      s <- table(x)
      return(as.vector(as.numeric(names(s))))
    }))
    
    resScale <- c(min(resList), max(resList))
    d <- list(list(name = scaleNames[index], items = names(fr), Reversed = names(rev),
                   SuggestedResponsScale = resScale))
    names(d) <- scaleNames[index]
    
    output <- append(output, d)
    
  }
  CheckResponsScale(output)
  PrintScaleItems(output)
  return(output)
}

write_csv <- function(x, ...) {
  UseMethod("write_csv", x)
}
write_csv.Psychometric <- function(object, items = T,  other = T, ...)
{
  getAllItemFrames <- function()
  {
    res <- NULL
    for (frames in object$ScaleItemFrames)
    {
      if (is.null(res))
        res <- frames
      else
        res <- cbind(res, frames)
    }
    return(as.data.frame(res))
  }
  data <- object$ScaleFrame
  if (items == T)
    data <- cbind(data, getAllItemFrames())
  if (other == T)
    data <- cbind(data, object$OtherVariables)
  write.csv(as.data.frame(data), ...)
}

save.Psychometric <- function(object, items = T, other = T, ...)
{
  write.csv(object, items, other, ...)
}




