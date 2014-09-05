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

























