# IS607 Assignment #1

#1) What versions of R and RStudio do you have installed?

sessionInfo()$R.version$version.string # on my machine this returns "R version 3.1.1 (2014-07-10)

rStudioVersion = 'v0.98.1028'

#2) What version of PostgreSQL do you have installed?

postgreSQLVersion = "PostgreSQL 9.3.5, compiled by Visual C++ build 1600, 64-bit"

#3) Install and load the R package DMwR. Load the data set sales and determine the number of observations 
#contained in the data set. (In RStudio, this is easy to determine.)

library(DMwR)
print(nrow(sales)) #401146




