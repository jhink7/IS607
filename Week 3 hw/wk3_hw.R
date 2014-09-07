#### Q1 ####
getmissingvals <- function(input) {

  ret <- length(which(is.na(input)))
  
  return <- ret
}

missval <- getmissingvals(c(1,4,NA,9,NA))

missval

#### Q2 ####
getmissingvalsfromDF <- function(input) {
  
  vectVals <- NULL
  colNames <- NULL
  for(i in 1:ncol(input))
  {
    vectVals[i] = getmissingvals(input[[i]])
    colNames[i] <- colnames(input)[i] 
  }
  names(vectVals) <- colNames
  
  return <- vectVals
}

df <- data.frame(c1 = c(1,2,3), c2 = c(4,5,6), c3 = c(7,8,NA))

missval2 <- getmissingvalsfromDF(df)
missval2

#### Q3 ####
getkeyvalues <- function(input) {
  
  missvals <- getmissingvals(input)
  inputCleaned <- input[!is.na(input)]
  
  trueLength <- length(input) - missvals
  sum <- sum(input, na.rm = TRUE)
  avg <- sum / trueLength
  
  sqds <- (inputCleaned - avg)^2
  sd <- sqrt(sum(sqds) / trueLength)
  
  
  # manually determine min and max
  max <- NULL
  min <- NULL
  for(i in 1:length(inputCleaned))
  {
    if(i==1)
    {
      max=inputCleaned[i]
      min=inputCleaned[i]
    }
    else
    {
      if(inputCleaned[i] > max)
      {
        max = inputCleaned[i]
      }
      
      if(inputCleaned[i] < min)
      {
        min = inputCleaned[i]
      }
    }
  }
  
  #manually determine mean, quartiles
  med <- NULL
  q1 <- NULL
  q3 <- NULL
  inputCleaned <- sort(inputCleaned)
  lgth <- length(inputCleaned)
  
  if(lgth %% 2 == 0)
  {
    index1 <- (length(inputCleaned)) / 2
    index2 <- (length(inputCleaned) + 2) / 2
    med <- (inputCleaned[index1] + inputCleaned[index2]) / 2
    
    halflength <- (length(inputCleaned)) / 2
    x <- seq_along(inputCleaned)
    halves <- split(inputCleaned, ceiling(x/halflength))
    
    if(halflength %% 2 == 0)
    {
      qindex1 <- (halflength) / 2
      qindex2 <- (halflength + 2) / 2
      q1 <- (halves[[1]][qindex1] + halves[[1]][qindex2]) / 2
      
      q3 <- (halves[[2]][qindex1] + halves[[2]][qindex2]) / 2
    }
    else
    {
      qindex <- (halflength + 1) / 2
      q1 <- halves[[1]][qindex]
      
      q3 <- halves[[2]][qindex]
    }
  }
  else
  {
    index <- (length(inputCleaned) + 1) / 2
    med <- inputCleaned[index]
    
    inputCleaned <- inputCleaned[-index]
    halflength <- (length(inputCleaned)) / 2
    x <- seq_along(inputCleaned)
    halves <- split(inputCleaned, ceiling(x/halflength))
    
    if(halflength %% 2 == 0)
    {
      qindex1 <- (halflength) / 2
      qindex2 <- (halflength + 2) / 2
      q1 <- (halves[[1]][qindex1] + halves[[1]][qindex2]) / 2
      
      q3 <- (halves[[2]][qindex1] + halves[[2]][qindex2]) / 2
    }
    else
    {
      qindex <- (halflength + 1) / 2
      q1 <- halves[[1]][qindex]

      q3 <- halves[[2]][qindex]
    }
    
  }

  return <- c(missing = missvals,mean = avg, med = med, q1 = q1, q3 =q3, sd = sd, max = max, min = min)
}

keyvalues <- getkeyvalues(c(6, 7, 15, 36, 39, 40, 41, 42, 43, 47, 49, 50, NA))
keyvalues











q1length <- index - 1
if(q1length %% 2 == 0)
{
  q1index1 <- (q1length) / 2
  q1index2 <- (q1length + 2) / 2
  med <- (inputCleaned[q1index1] + inputCleaned[q1index2]) / 2
}
else
{    
  q1index <- (q1length + 1) / 2
  q1 <- inputCleaned[q1index]
}

q3length <- length(inputCleaned) - (index)
q3startIndex <- index + 1
q3EndIndex <- length(inputCleaned)
#print(q3length)

