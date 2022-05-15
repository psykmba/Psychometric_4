#' Getting the Psychometric class
#'
#' Makes it simple to do basic psychometrics
#' @param data A dataframe with the items and eventual extra variables
#' @param scaleNames A vector with the names of the scales 
#' @param responseScale A list with vectors of response intervalls, either one (if all are the same) or one for each scale
#' @param typeSum A functions used for summarizing the items to scales
#' @param itemLength The number of significant characters in items
#' @param reverse A boolean that sets whether items ending with 'R' should be reversed
#' @param idVar The name of the case identifier (if none a case number variable called ID will be created)
#' @param name The name of the object (optional)
#' @param itemDictionary a textfile with information about scale, items and sign
#' @param itemList a textfile with information about scale, items and sign
#' @param reverseList a textfile with information about scale, items and sign
#' @return A Psychometric object that can be used for analyses
#' @export
#' 
GetPsychometric <- function(data, scaleNames, responseScale = list(c(1,5)),
                            typeSum = "Mean", itemLength = 6,  #item skall ha samma namn som skalan plus tecken
                            reverse = T, idVar = "ID", name = "Psychometric",
                            itemDictionary = NULL, itemList=NULL, reverseList=c())
{
  # if there is a variable called "ID" this variable is added to all the data
  # frame
  IDVar <- NULL
  if (!is.null(data[[idVar]]))
  {
    IDVar <- data[[idVar]]
    data <- dplyr::select(data, -idVar)
  }
  else
  {
    IDVar <- as.data.frame(1:nrow(data))
    
  }
  if (FALSE %in% sapply(scaleNames, FUN = function(x) return(nchar(x) >= itemLength)))
  {
    print(paste("Error: itemLength = ", itemLength, "is larger than the string length of the shortes scale name"))
    return()
  }
  CreateItemNames <- function()
  {
    index <- 1
    for (scale in scaleNames)
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
  
  expandResponsScale <- function(respons, scales)
  {
    l <- length(respons) # Number of respons types
    s <- length(scales) # Number of scales
    if (l > 1 && l != s)
      warning("Respons scale is not 1 and not the same as the number of scales using the first for all", 
              immediate. = T)
    if (l == 1 || l != s)
      ret <- rep(respons[1], s)
    else 
      ret <- respons
    names(ret) <- scales
    return(ret)
  }
  
  GetNonItemVar <- function()
  {
    res <- NULL
    if (!is.null(IDVar))
    {
      res <- IDVar
      
    }
    else
    {
      res <- 1:nrow(data)
    }  
    res <- as.data.frame(res)
    names(res) <- "ID"
    
    scaleNamesItemLength <- unlist(sapply(scaleNames, FUN = function(x)
      return(substr(x, 1, itemLength))), use.names = F)
    for (index in ncol(data):1)
    {
      name <- names(data[index])
      if (!(substr(name, 1, itemLength) %in% scaleNamesItemLength))
      {
        res <- c(res, data[index])
        data <- data[-index]
      }
    }
    return(data.frame(res))
    
  }
  getSignItemName <- function(x, scales, itemLength)
  {
    signPart <- strtrim(scales, itemLength)
    
    if (nchar(scales) > itemLength)
    {
      newNames <- NULL
      for (item in 1:ncol(x))
      {
        c <- ""
        iName <- names(x[item])
        for (s in itemLength+1:nchar(scales))
        {
          
          if (substring(scales, s, s) == substring(iName, s,s))
          {
            c <- paste(c, substring(scales, s, s), sep = "")
          }
          else
            break
        }
        if (c != "")
          newNames <-c(newNames, str_remove(names(x[item]),c))
        else
          newNames <- c(newNames, names(x[item]))
      }
      names(x) <- newNames
    }
    return(x)
  }
  ChangeOriginalDataNames <- function(data)
  {
    signPart <- strtrim(scaleNames, itemLength)
    for(v in names(data))
    {
      if (nchar(v) > itemLength)
      {
        if (substr(v, 1, itemLength) %in% signPart)
        {
          iName <- scaleNames[match(substr(v, 1, itemLength),signPart)]
          if (nchar(iName) > itemLength)
          {
            c <- ""
            for (s in itemLength+1:nchar(v))
            {
              
              if (substring(v, s, s) == substring(iName, s,s))
              {
                c <- paste(c, substring(v, s, s), sep = "")
              }
              else
                break
            }
            if (c != "")
              names(data)[names(data) == v] <- str_remove(v,c)
            else
              names(data)[names(data) == v] <- v
            
          }
        }
      }
      
    }
    return(data)
  }
  GetScaleItemFrames <- function(d, responses)
  {
    
    GetData <- function(name)
    {
      frame <- dplyr::select(d, dplyr::starts_with(substr(name, 1, itemLength)))
      return(frame)
    }
    Reverse <- function(col, resp)
    {
      if (reverse == T)
        return((resp[1]+resp[2]) - col)
      else
        return(col)
    }
    GetReverse <- function(frame, resp)
    {
      #      print(resp[[1]])
      for(index in 1:length(frame))
      {
        name <- names(frame[index])
        frame[index] <- ifelse(substr(name, nchar(name), nchar(name)) == "R", 
                               Reverse(frame[index], resp[[1]]),
                               frame[index])
      }
      return(frame)
    }
    resFrames <- NULL
    for(index in 1:length(scaleNames))
    {
      interm <- GetData(scaleNames[index])
      interm <- getSignItemName(interm,scaleNames[index], itemLength )
      resFrames <- append(resFrames, list(as.data.frame(GetReverse(interm, responses[index]))))
    }
    names(resFrames) <- scaleNames
    return(resFrames)
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
  GetDictionary <- function()
  {
    d <- NULL
    if (file.exists(itemDictionary))
      d <- read.delim(itemDictionary, comment.char="#")
    else
    {
      print("Dictionary file does not exist")
      return(NULL)
    }
    rowNames <- NULL
    for(index in 1:nrow(d))
      rowNames <- c(rowNames, paste(substr(d[index, 4],1,itemLength), d[index,1], sep = ""))
    return(data.frame(cbind(d[2], d[3], d[4], row.names = rowNames)))
  }
  if (!is.null(itemList))
    data <- CreateItemNames()
  otherVariables <- GetNonItemVar()
  responseScale <- expandResponsScale(responseScale, scaleNames)
  scaleItemFrames <- GetScaleItemFrames(data, responseScale)
  scaleFrames <- GetScalesFrame(scaleItemFrames, scaleNames)
  data <- ChangeOriginalDataNames(data)
  names(scaleItemFrames) <- scaleNames
  if (!is.null(itemDictionary)) 
    itemDictionary <- GetDictionary()
  else
    itemDictionary <- NULL;
  MyObject <- list(ResponseScales = responseScale, ScaleItemFrames = scaleItemFrames, ScaleFrame = scaleFrames, 
                   ScaleNames = scaleNames, OtherVariables = otherVariables, OriginalData = data, 
                   Name = name, ItemDictionary = itemDictionary,
                   ItemLength = itemLength, ResultList = list(), RCommands = list())
  
  class(MyObject) <- "Psychometric"
  return(MyObject)
  
}


