#### Q1 ####

# Question 1: How does the preference between the two dishes vary with age?
# Question 2: How does the preference between the two dishes vary by city?
# Question 3: What percentage of people in each city voted?

#### Q2 ####
messy <- data.frame(c1 = c("","","Yes", "No"), c2 = c("Edinburgh","16-24","80,100", "35,900"), c3 = c("Edinburgh","25+","143,000", "214,800"), c4 = c("Glasgow","16-24","99,400", "43,000"), c5 = c("Glasgow","25+","150,400", "207,000"))

#### Q3 ####
library(tidyr)
library(plyr)
library(dplyr)

data <- messy[3:4,] %>% gather(yesno, votes, c2:c5) %>% select(-yesno)
city1 <- c(t(as.vector(messy[1,])[-1][1:2]), t(as.vector(messy[1,])[-1][1:2]))
city2 <- c(t(as.vector(messy[1,])[-1][3:4]), t(as.vector(messy[1,])[-1][3:4]))
data$city <- c(city1, city2)

agebucket1 <- c(t(as.vector(messy[2,])[-1][1:1]))
agebucket2 <- c(t(as.vector(messy[2,])[-1][2:2]))
data$age <- c(agebucket1,agebucket1, agebucket2, agebucket2,agebucket1,agebucket1, agebucket2, agebucket2)
data$votes <- extract_numeric(data$votes)

#### Q4 ####
#1
byCity <- ddply(data, .(city, c1), summarize, s1 = sum(votes))
EdinPrefer <- byCity[2,3] / (byCity[2,3] + byCity[1,3])
GlasgPrefer <- byCity[4,3] / (byCity[4,3] + byCity[3,3])
#Answer: 47.7% of Edinburgh preferred Cullen skink while 49.97% of Glasgow voters preferred Cullen skink

#2
byAge <- ddply(data, .(age, c1), summarize, s1 = sum(votes))
youngPrefer <- byAge[2,3] / (byAge[2,3] + byAge[1,3])
oldPrefer <- byAge[4,3] / (byAge[4,3] + byAge[3,3])
#Answer: 69.4% of younger voters (16-24) preferred Cullen skink while 41.1% of older voters (25+) preferred Cullen skink

#3
turnout <- ddply(data, .(city), summarize, s1 = sum(votes))

edinTurn <- turnout[1,2] / 487300 #number found from wikipedia
glasgTurn <- turnout[2,2] / 596550
#Answer: There was a better voter turnout in edinburgh

#### Q5 ####
#Answer:  Having gone through the exercise, I think it would have been beneficial to have yes and no votes for each city/agebucket pair on the same row of the dataframe.  My first and second
# questions in particularly could have been answered more elegantly if I had structured the data in this way.



