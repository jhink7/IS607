setwd("C:/Users/Justin/Documents/GitHub/IS607/Project 2")

library(ggplot2)
library(plyr)
library(data.table)
library(dplyr) 

masterHitters = read.csv("Hitters.csv") 

hitterTable <- data.table(masterHitters)

#### Offense By Year ####
yearlyOffenseProxyTable <- hitterTable[, sum(R) / sum(PA), by = Season]
yearlyOffenseProxy <- data.frame(Season = yearlyOffenseProxyTable[[1]], Offense = yearlyOffenseProxyTable[[2]])

p1 <- ggplot(yearlyOffenseProxy, aes(x= Season, y = Offense)) + geom_bar(stat = "identity")
p1 + annotate("text", x = 2000, y =0.16,  label = "Steroids") + annotate("text", x = 1969, y =0.16,  label = "Mound Lowered") + annotate("text", x = 1908, y =0.16,  label = "Dead Ball Era")

modernOffenseProxy <- subset(yearlyOffenseProxy, Season > 1980)
p2 <- ggplot(modernOffenseProxy, aes(x= Season, y = Offense)) + geom_bar(stat = "identity")
p2 + annotate("text", x = 2000, y =0.16,  label = "Steroids")

#### Offense By Team ####
teamOffenseProxyTable <- hitterTable[, sum(R) / sum(PA), by = Team]
setnames(teamOffenseProxyTable, c("Team","Offense") )

teamOffenseProxyTable$Team3 <- reorder(teamOffenseProxyTable$Team, teamOffenseProxyTable$Offense)

p2 <- ggplot(teamOffenseProxyTable, aes(y=Offense))
p2 + geom_bar(aes(x=Team3), data=teamOffenseProxyTable, stat="identity")

#### Modern Offense Per Team ####
modernHitterTable = subset(hitterTable, Season > 1980)
teamOffenseProxyTable <- modernHitterTable[, sum(R) / sum(PA), by = Team]
setnames(teamOffenseProxyTable, c("Team","Offense") )

teamOffenseProxyTable$Team3 <- reorder(teamOffenseProxyTable$Team, teamOffenseProxyTable$Offense)

teamOffenseProxyTable <- arrange(teamOffenseProxyTable,desc(Offense))

p2 <- ggplot(teamOffenseProxyTable, aes(y=Offense))
p2 + geom_bar(aes(x=Team3), data=teamOffenseProxyTable, stat="identity")

print(teamOffenseProxyTable)

#### Year to Year Correlations ####
masterHitters2 <- masterHitters
masterHitters2$Season <- masterHitters2$Season + 1

year2year <- merge(masterHitters2, masterHitters, by=c("playerid", "Season"))

corrAvg <- cor(year2year$AVG.x, year2year$AVG.y)
corrRBI <- cor(year2year$RBI.x, year2year$RBI.y)
corrR <- cor(year2year$R.x, year2year$R.y)

corrOBP <- cor(year2year$OBP.x, year2year$OBP.y)
corrSLG <- cor(year2year$SLG.x, year2year$SLG.y)

corrwRCPlus <- cor(year2year$wRCPlus.x, year2year$wRCPlus.y)
corrHRPerFB <- cor(na.omit(year2year)$HR.FB.x, na.omit(year2year)$HR.FB.y)
corrBBPct <- cor(year2year$BBPct.x, year2year$BBPct.y)
corrKPct <- cor(na.omit(year2year)$KPct.x, na.omit(year2year)$KPct.y)


