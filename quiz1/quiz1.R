#1
vect <- c(1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5)

#2
charvect <- as.character(vect)

#3
factors <- factor(vect)

#4
numLevels <- nlevels(factors)

#5
fn <- 3 * vect ^ 2  - 4 * vect + 1

#6

xFlat <- c(1,1,1,1,1,1,1,1,5,4,6,2,3,2,7,8,8,9,4,7,4,9,6,4) 
X <- matrix(xFlat,8,3)
Y <- c(45.2, 46.9, 31.0, 35.3, 25.0,43.1,41.0,35.1)

B <-  solve((t(X)%*%X)) %*% t(X) %*% Y

#7
v = c("Derek", "Jeter", "Shortstop", "Yankees", 2)
names(v) = c("First", "Last", "Position", "Team", "Number") 
v["First"]
v["Last"]
v["Position"]
v["Team"]
v["Number"]

#8

c1 <- 1:10
c2 <- c("first", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th", "10th")
c3 <- factor(1:5)

df <- data.frame(C1 = c1, C2 = c2, C3 = c3)
df

#9
c1new = c(11)
c3new = factor(c1new)
r1 = data.frame(C1 = c1new, C2 ="11th", C3 = factor(c1new))
df = rbind(df, r1)

#10
temps <- read.table(file = "temperatures.csv", header = TRUE, sep = ",")

#11
temps <- read.table(file = "C:/Users/Justin/Documents/temperatures.txt", header = TRUE, sep = "\t")

#12
temps <- read.table(file = "http://www.temprepo/citytemps.txt", header = TRUE, sep = "|")

#13
fact = 1
for(i in 1:12)
{
  fact = fact * i
}

fact

#14
balance = 1500
for(i in 1:72)
{
  balance = balance * 1.032
}

balance

#15
vect = 1:20
sum = 0
count = 1
for(i in 1:20)
{
  if(count == 3)
  {
    sum = sum + i
    count = 1
  }
  else
  {
    count = count + 1
  }
}

sum

#16
sum = 0
for(i in 1:10)
{
  sum = sum + 2 ^ i
}

sum

#17
sum = 0
i = 1
while (i <= 10)
{
  sum = sum + 2 ^ i
  i = i + 1
}

sum

#18
vect = 1:10
temp = 2 ^ vect
sum(temp)

#19
vect = c(20)

count = 1
for(i in 21:50)
{
  if(count == 5)
  {
    vect = append(vect, i)
    count = 1
  }
  else
  {
    count = count + 1
  }
}

vect

#20
testWord = "example"
cVect = c(testWord)

for(i in 1:9)
{
  cVect = append(cVect, testWord)
}

cVect

#21
getroots <- function(a,b,c) {
  
  if(a > 0)
  {
    disc = b^2 - 4*a*c
    
    if(disc < 0)
    {
      result <- c("No Real Roots")
    }
    else if(disc == 0)
    {
      r1 = -b/(2*a)
      result <- c(r1)
    }
    else
    {
      r1 = (-b + sqrt(b^2 - 4*a*c)) / (2*a)
      r2 = (-b - sqrt(b^2 - 4*a*c)) / (2*a)
      
      result <- c(r1, r2)
    }
  }
  else
  {
    result <- c("Not a Quadratic")
  }
  
  return <- result
}

roots <- getroots(1,-2,-4)
roots




















































