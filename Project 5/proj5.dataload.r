#Use RNeo4j
library(RNeo4j)

graph = startGraph("http://localhost:7474/db/data/")

#clear(graph)

#set working directory to whatever you need
setwd("C:/Users/Justin/Documents/GitHub/IS607/Project 2")

library(ggplot2)
library(plyr)
library(data.table)
library(dplyr) 
library(boot) 
library(reshape2) 

#read in hitters data.  Same file as used in project #2
masterHitters = read.csv("Hitters.csv") 


#create player nodes
uniquePlayers = unique(masterHitters$Name)

query = "
MERGE (p:Player {name:{player_name}})
"

t = newTransaction(graph)

for (i in 1:length(uniquePlayers)) {
  player_name = uniquePlayers[i]
  
  appendCypher(t, 
               query, 
               player_name = player_name)
}

commit(t)

#create team nodes
uniqueTeams = unique(masterHitters$Team)

t2 = newTransaction(graph)

query = "
MERGE (tm:Team {name:{team_name}})
"

for (i in 1:length(uniqueTeams))
{
  team_name = uniqueTeams[i]
  appendCypher(t2, 
               query, 
               team_name = team_name)
}

commit(t2)

#create "played for" relationship
t3 = newTransaction(graph)

query = "
match (p:Player {name: {player_name}}), (tm:Team {name: {team_name}}) create (p) - [r:playedfor {year: {year}}]-> (tm)
"

for (i in 1:nrow(masterHitters)) {
  player_name = masterHitters[i, ]$Name
  team_name = masterHitters[i, ]$Team
  year = masterHitters[i, ]$Season
  
  appendCypher(t3, 
               query, 
               player_name = player_name, 
               team_name = team_name, 
               year = year)
}

commit(t3)