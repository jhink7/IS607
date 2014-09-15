library(ggplot2)
library(plyr)

#### Q1 ####
data <- movies
data <- arrange(data, year)
data$decade <- apply(data,1,function(row) as.factor(as.numeric(row[2]) %/%10 * 10))


ggplot(data = data) + geom_histogram(aes(x = decade))

#### Q2 ####

# All Genres
# In general, ratings peaked in the 1920s and have plateued at a slightly
# lower value since that time
meanrating <- tapply(data$rating, data$decade, mean)
decade <- factor(levels(data$decade), levels = levels(data$decade))
qplot(decade, meanrating)
qplot(decade, meanrating, geom="bar", stat="identity")

#Action
#Action ratings peaked in the 1920s and have declined since then.  A recent
#uptick in the 2000s appears but it's unclear whether this is a trend as the
#magnitude of the change is within the reasonable amount of random variation
actdata <- subset(data, Action==1)
meanrating <- tapply(actdata$rating, actdata$decade, mean)
decade <- factor(levels(actdata$decade), levels = levels(actdata$decade))
qplot(decade, meanrating)
qplot(decade, meanrating, geom="bar", stat="identity")

#Animation
#Animation ratings have plateued since the 1920s
andata <- subset(data, Animation==1)
meanrating <- tapply(andata$rating, andata$decade, mean)
decade <- factor(levels(andata$decade), levels = levels(andata$decade))
qplot(decade, meanrating)
qplot(decade, meanrating, geom="bar", stat="identity")

#Comedies
#Comedy ratings peaked int the 1920s and have plateued at a slightly lesser value since then
comdata <- subset(data, Comedy==1)
meanrating <- tapply(comdata$rating, comdata$decade, mean)
decade <- factor(levels(comdata$decade), levels = levels(comdata$decade))
qplot(decade, meanrating)
qplot(decade, meanrating, geom="bar", stat="identity")

#Drama
#Drama ratings peaked in the 1920s and have plateued since then
dramadata <- subset(data, Drama==1)
meanrating <- tapply(dramadata$rating, dramadata$decade, mean)
decade <- factor(levels(dramadata$decade), levels = levels(dramadata$decade))
qplot(decade, meanrating)
qplot(decade, meanrating, geom="bar", stat="identity")

#Documentary
# Documentary ratings are trending slightly upward as time goes on
docdata <- subset(data, Documentary==1)
meanrating <- tapply(docdata$rating, docdata$decade, mean)
decade <- factor(levels(docdata$decade), levels = levels(docdata$decade))
qplot(decade, meanrating)
qplot(decade, meanrating, geom="bar", stat="identity")



##### Q3 #####
#view with length outliers included doesn't tell us much.
ggplot(data = data, aes(x=length, y=rating)) + geom_point()

#Only look at movies of 3 hours in length or shorter
#There still appears to be little relationship between length and rating
#One thing: Very long movies appear unlikely to have horrible ratings (I use 'horrible' as an arbitrary proxy for rating < 2.5)
filteredLengthData <- subset(data, length < 180)
ggplot(data = filteredLengthData, aes(x=length, y=rating)) + geom_point() + geom_smooth(method = "lm", se=FALSE, color="red", aes(group=1))

##### Q4 #####
# The mean ratings of each movie are different, there is probably some predictive power here
# Docs have the highest avg rating, action have the lowest.  This isn't surprising.
mratingsbyGenre <- c(Action = mean(actdata$rating), Animation = mean(andata$rating), Comedy = mean(comdata$rating), Drama = mean(dramadata$rating) , Doc = mean(docdata$rating))
df <- data.frame(ratings = mratingsbyGenre, genres = names(mratingsbyGenre))
meanr <- tapply(df$ratings, df$genres, mean)
genre <- factor(levels(df$genres), levels = levels(df$genres))
qplot(genre, meanr, geom="bar", stat="identity")

##### Q5 #####
# Given the assumed scope of this assignment, I'm assuming correlation = predictive power.  Obviously this is not very robust and a large amount of linearity is assumed.
# Correlation != causation, but it can be a hint.  Good enough for now.
# Looks like budget is the best predictor.  Narrative: High budget = high marketing budget = greater exposure = more people seeing the movie = more votes on IMDB.  
func <- function(row){
  retval = NA
  if(row[17] == "NC-17")
  {
    retval = 4
  }
  else if(row[17] == "R")
  {
    retval = 3
  }
  else if(row[17] == "PG-13")
  {
    retval = 2
  }
  else if(row[17] == "PG")
  {
    retval = 1
  }
  return(retval) }

data$mpaabucket <- apply(data,1,function(row) func(row))
rvector <- c(year = cor(data$year, data$votes), decade = cor(as.numeric(data$decade), data$votes), budget = cor(data$budget, data$votes, use = "complete.obs"), length = cor(data$length, data$votes, use = "complete.obs"), length = cor(data$mpaabucket, data$votes, use = "complete.obs"))
rvector








