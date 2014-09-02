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