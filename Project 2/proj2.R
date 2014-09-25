setwd("C:/Users/Justin/Documents/GitHub/IS607/Project 2")

library(ggplot2)
library(plyr)
library(data.table)
library(dplyr) 
library(boot) 
library(reshape2) 

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

#### Weighted Year to Year Correlations ####
wCorrAvg <- corr(cbind(year2year$AVG.x, year2year$AVG.y), w=(as.numeric(year2year$PA.x) + as.numeric(year2year$PA.y)))
wCorrRBI<- corr(cbind(year2year$RBI.x, year2year$RBI.y), w=(as.numeric(year2year$PA.x) + as.numeric(year2year$PA.y)))
wCorrR <- corr(cbind(year2year$R.x, year2year$R.y), w=(as.numeric(year2year$PA.x) + as.numeric(year2year$PA.y)))

wCorrOBP<- corr(cbind(year2year$OBP.x, year2year$OBP.y), w=(as.numeric(year2year$PA.x) + as.numeric(year2year$PA.y)))
wCorrSLG <- corr(cbind(year2year$SLG.x, year2year$SLG.y), w=(as.numeric(year2year$PA.x) + as.numeric(year2year$PA.y)))
                                                       
wCorrwRCPlus <- corr(cbind(year2year$wRCPlus.x, year2year$wRCPlus.y), w=(as.numeric(year2year$PA.x) + as.numeric(year2year$PA.y)))
wHRPerFB<- corr(cbind(na.omit(year2year)$HR.FB.x, na.omit(year2year)$HR.FB.y), w=(as.numeric(na.omit(year2year$PA.x) + as.numeric(na.omit(year2year$PA.y)))))
wBBPct <- corr(cbind(year2year$BBPct.x, year2year$BBPct.y), w=(as.numeric(year2year$PA.x) + as.numeric(year2year$PA.y)))
wKPct <- corr(cbind(na.omit(year2year)$KPct.x, na.omit(year2year)$KPct.y), w=(as.numeric(na.omit(year2year$PA.x) + as.numeric(na.omit(year2year$PA.y)))))

f<-data.frame(c("Avg","RBI","R","OBP","SLG","wRC+","HRPerFB","BBPct","KPct"), c(wCorrAvg, wCorrRBI, wCorrR, wCorrOBP, wCorrSLG, wCorrwRCPlus, wHRPerFB, wBBPct,wKPct))
setnames(f, c("Stat","R") )
f[with(f, order(-R)), ]
print(f)

#### Defensive Spectrum to Offense Correlation ####
defSpecToOffCorr <- corr(cbind(masterHitters$Pos, masterHitters$wRCPlus), w=(masterHitters$PA))

p3 <- ggplot(masterHitters, aes(x=Pos, y=wRCPlus)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)

p3

#### Defensive Spectrum to WAR Correlation ####
defSpecToWARCorr <- corr(cbind(masterHitters$Pos, masterHitters$WAR), w=(masterHitters$PA))

p4 <- ggplot(masterHitters, aes(x=Pos, y=WAR)) +
  geom_point(shape=1) +    # Use hollow circles
  geom_smooth(method=lm)

p4


#### Correlations Matrix ####

noNAHitters <- filter(masterHitters, !is.na(KPct))

d <- data.frame(KPct = noNAHitters$KPct, BBPct = noNAHitters$BBPct, HR = noNAHitters$HR, RBI = noNAHitters$RBI, R = noNAHitters$R, wRCPlus = noNAHitters$wRCPlus, Off = noNAHitters$Off, Def = noNAHitters$Def, WAR = noNAHitters$WAR)

d_cor <- as.matrix(cor(d))
d_cor_melt <- arrange(melt(d_cor), -abs(value))

c <- ncol(d)
r <- nrow(d_cor_melt)
numTail <- r- c
d_cor_melt<- tail(d_cor_melt, numTail)

Nth.delete<-function(dataframe, n)dataframe[-(seq(n,to=nrow(dataframe),by=n)),]

corFrame <- data.frame(d_cor_melt)
row.names(corFrame) <- seq(nrow(corFrame))
corFrame <- Nth.delete(corFrame, 2)



