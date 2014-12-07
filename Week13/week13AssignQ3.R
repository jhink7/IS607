library(Lahman)
library(plyr)
library(microbenchmark)

library(dplyr)

aggregateHR_PLYR <- function() {
  games <- ddply(Batting, "playerID", summarise, total = sum(HR))
  sorted<-head(arrange(games, desc(total)), 5)
  #return <-1
}

aggregateHR_DPLYR <- function() {
  players <- group_by(Batting, playerID)
  games2 <- summarise(players, total = sum(HR))
  sorted2<-head(arrange(games2, desc(total)), 5)
  #return <-sorted2
}

bench <- microbenchmark(
  aggregateHR_PLYR()
  ,aggregateHR_DPLYR()
  ,times=1L
)

print(bench) 
boxplot(bench) 
library(ggplot2) 
qplot(y=time, data=bench, colour=expr) + scale_y_log10()




