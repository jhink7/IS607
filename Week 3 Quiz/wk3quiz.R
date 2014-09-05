#### Q1 ####

getavg <- function(a) {
  
  result = mean(a)
  return <- result
}

avg <- getavg(1:8)
avg

#### Q2 ####

getavg <- function(a) {
  
  result = mean(a, na.rm = TRUE)
  return <- result
}

avg <- getavg(c(1,2,NA))
avg

#### Q3 ####
getgcd <- function(x,y) {
  #check inputs
  temp <- x%%y
  #recursion
  return(ifelse(temp, getgcd(y, temp), y))
}

gcd <- getgcd(24, 88)
gcd

#### Q4 ####

getEucGCD <- function(x,y) {

  while (x != y)
  {
    if(x > y)
    {
      x = x - y
    }
    else
    {
      y = y - x
    }
  }
  return(x)
}

gcd <- getEucGCD(24, 88)
gcd

#### Q5 ####
docalc <- function(x,y) {
  
  r <- x^2*y + 2*x*y - x*y^2
  return (r)
}

calc <- docalc(34,12)
calc

#### Q6 ####
makemodels = read.csv("week-3-make-model-data.csv") 
prices = read.csv("week-3-price-data.csv") 

merged = merge(makemodels, prices, "ModelNumber") 
#Answer: 27 results. Without looking at the data I would have guessed the number of resulting obersvations to be
# max(length(makemodels), length(prices))

#### Q7 ####
merged = merge(makemodels, prices, "ModelNumber", all = TRUE) 

#### Q8 ####
sub2010 <- subset(merged, Year==2010)

#### Q9 ####
subCost <- subset(merged, Price >10000)
subCost

#### Q10 ####
trimmedSubCost = subCost[ , -which(names(subCost) %in% c("ModelNumber", "Color"))]
trimmedSubCost

#### Q11 ####
charCount <- function(x) {
  
  r <- nchar(x)
  return (r)
}

charCountV <- charCount(c("item1", "item22", "item333"))
charCountV

#### Q12 ####
doconcat <- function(x,y) {
  
  if(length(x) != length(y)){ return ("Vectors not the same length")}
  
  r <- paste(x, y, sep = " ")
  return (r)
}

concatenated <- doconcat(c("aa", "bb", "cc"),c("xx", "yy", "zz"))
concatenated

#### Q13 ####
getvowelsub <- function(x) {
  
  vowels <- c("a","e","i","o","u","A","E","I","O","U")

  firstindex <- 999999 #something artificially high
  for(i in vowels)
  {
    currIndex <- regexpr(i,x, fixed=T)[1]
    
    if(currIndex != -1 && currIndex < firstindex)
    {
      firstindex = currIndex
    }
  }
  r <- NULL
  if(firstindex < 999999){
    if(firstindex < nchar(x[1]) - 1){
      r <- substr(x[1], firstindex, firstindex+2)
    }
    else
    {
      r <- "First vowel too close to end of string"
    }
    
  }
  else
  {
    r <- "No Vowels Found"
    
  }
  return (r)

}

subs <- getvowelsub(c("TxsaXat"))
subs

#### Q14 ####
d <- c(12,25,2)
m <- c(1,3,10)
y <- c(2010,2012,2014)

df <- data.frame(d,m,y)

df["DateTime"] <- NA
df$DateTime <- ISOdatetime(df$y, df$m, df$d,0,0,0)

#### Q15 ####
date <- as.Date("9-12-2014", "%m-%d-%Y")
date

#### Q16 ####
month <- as.POSIXlt(date)$mon + 1
month

#### Q17 ####

s <- seq(as.Date("2005/1/1"), as.Date("2014/12/31"), "days")
s


























