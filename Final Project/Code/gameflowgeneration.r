library(nhlscrapr)
library(RODBC)
library(ggplot2)
library(reshape2)

scheduleFileLocation ="C:/CUNY/IS607/Final Project/sched.csv"
postgreSQLconnectionString = "driver={PostgreSQL ANSI(x64)};server=localhost;database=CUNY607;uid=Justin;pwd=password"
processgame <- function(gID)
{ 
  #gID <- 20107
  # download gamefiles from NHL.com and process the data into r dataframes
  gamedata = process.single.game (season="20142015",
                           gcode=toString(gID),
                           rdata.folder="nhlr-data",
                           override.download=FALSE,
                           save.to.file=TRUE)
  
  # Get the play by play event log. It comes to us as dataframe
  pbpRaw<-gamedata$playbyplay
  
  # Exclude events from a shootout (if there is one)
  pbpRaw = subset(pbpRaw, period<5)
  
  # Home and away teams labels found here
  awayteam <- gamedata[3]$teams[1]
  hometeam <- gamedata[3]$teams[2]
  
  pbpRaw$isHomeShEvent = (pbpRaw$etype=="SHOT"|pbpRaw$etype=="GOAL"|pbpRaw$etype=="MISS"|pbpRaw$etype=="BLOCK") & pbpRaw$ev.team==hometeam
  pbpRaw$isHomeShEvent = pbpRaw$isHomeShEvent * 1
  
  pbpRaw$isAwayShEvent = (pbpRaw$etype=="SHOT"|pbpRaw$etype=="GOAL"|pbpRaw$etype=="MISS"|pbpRaw$etype=="BLOCK") & pbpRaw$ev.team==awayteam
  pbpRaw$isAwayShEvent = pbpRaw$isAwayShEvent * 1
  
  # create running totals of events we are concerned with for both home and away teams
  pbpRaw$homeCummulative = cumsum(pbpRaw$isHomeShEvent)
  pbpRaw$awayCummulative = cumsum(pbpRaw$isAwayShEvent)
  
  # extract goal only events.  Will be used at the end of the plotting step
  goalEventsHome = subset(pbpRaw, (etype=="GOAL")&event!=1&ev.team==hometeam, select=c(seconds))$seconds
  goalEventsAway = subset(pbpRaw, (etype=="GOAL")&event!=1&ev.team==awayteam, select=c(seconds))$seconds
  
  # subset the data to what we need only
  pbpraw_sub = pbpRaw[,c("seconds","homeCummulative","awayCummulative")]
  colnames(pbpraw_sub) <- c("seconds",hometeam, awayteam)
  
  # wide to long with reshape2, ty HW
  dd = melt(pbpraw_sub, id=c("seconds"))
  plot = ggplot(dd) + geom_line(aes(x=seconds, y=value, colour=variable)) +
    scale_colour_manual(values=c("red","black")) + scale_y_continuous(name="Corsi Events") + scale_x_continuous(name="Gametime (seconds), Goal Events Dashed")
  
  title = sprintf("Gameflow %s @ %s", awayteam, hometeam) 
  plot + theme(legend.title=element_blank()) + ggtitle(title) + geom_vline(xintercept = goalEventsHome, color="red", size = 0.8, linetype="longdash" )+ geom_vline(xintercept = goalEventsAway, color="black", size = 0.8, linetype="longdash" )
  
  #save image to our working directory
  fileName=paste(gID,".png")
  ggsave(file=fileName)
  
  # Load data into PostgreSQL for future analysis
  channel = odbcDriverConnect(postgreSQLconnectionString)

  sqlSave(channel, pbpRaw, rownames = F, addPK = F, append = T)

  #clean up connection
  odbcClose(channel)
  print(paste("Processed ", gID))
}

#Get yesterday's date as a string
yest<-strftime(Sys.Date()-1)

#import our schedule from a csv file
sched = read.csv(scheduleFileLocation, header = TRUE)

#only process yesterday's games
yestGameIDs = subset(sched, GameDate==yest)$GameID

#process each game in our subset (yesterday's games)
for(gID in yestGameIDs){
  result <- tryCatch({
    processgame(gID)
  }, warning = function(war)
  {
    return(0)
  }, error = function(err)
  {
    return (0)
  }, finally = {
    
  })
  
}

