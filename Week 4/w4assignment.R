library(ggplot2)
library(plyr)
library(data.table)

#### Q1 ####

## I'm using the # of votes as a measure of movie popularity.  It logically follows that the more popular a movie is, the more people will want to discuss it
## (and opine in the form of ratings) on IMDB.
## I'm defining a popular move as one that has gathered one standard deviation (yearly) of votes above the mean (yearly again).  We can tweak the popularity
## parameter here by increasing this amount to something like mean + 1.5 sd or mean + 2 sd if we want to find "Really" popular movies
## I'm using rating as a measure of movie quality.  This is the most obvious proxy to me.  The idea of combining rating and budget in some fashion crossed my mind
## but having seend Michael Bay's work, in the end I didn't think this was wise.

## By the definitions above, 1927 had the best popular movies on average. http://en.wikipedia.org/wiki/1927_in_film

data <- movies
data <- arrange(data, year)

DT <- data.table(data)
popularThresholds <- DT[,mean(votes) + sd(votes), by = year]

yearlyThresholds <- data.frame(year = popularThresholds[[1]], popularThresholds[[2]])

m1 <- merge(data, yearlyThresholds, by.x = "year", by.y = "year")

m1$ispopular <- m1[[6]] > m1[[25]]

popularmovies <- subset(m1, ispopular == TRUE)

DTPopOnly <- data.table(popularmovies)
avgRatingsofPop <- DTPopOnly[, mean(rating), by = year]
setnames(avgRatingsofPop, c("year","rating") )
avgRatingsofPop <- avgRatingsofPop[order(-rating)]

## ggplot requirement
ggplot(avgRatingsofPop, aes(x= year, y = rating)) + geom_bar(stat = "identity")

### To satisfy the plyr package requirement, the following statement could have been substituted for the line avgRatingsofPop <- DTPopOnly[, mean(rating), by = year]
avgRatingsofPop<-ddply(DTPopOnly, "year", summarise, rating = mean(rating))



