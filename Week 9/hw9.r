library(rmongodb)

mongo <- mongo.create(db="unitedstates")

if (mongo.is.connected(mongo)) {
  buf <- mongo.bson.buffer.create()
  
  mongo.bson.buffer.append(buf, "abbr", "DC")
  
  query <- '{"state":{ "$exists": true }}'
  states <- mongo.find.all(mongo, "unitedstates.info", query, limit=100L)
  
  query <- '{"federal_district":{ "$exists": true }}'
  districts <- mongo.find.all(mongo, "unitedstates.info", query, limit=100L)
  
  query <- '{"territory":{ "$exists": true }}'
  territories <- mongo.find.all(mongo, "unitedstates.info", query, limit=100L)

}

mongo.destroy(mongo)

dfStates <- data.frame(states)
dfStates$X_id <-NULL

dfDistricts <- data.frame(districts)
dfDistricts$X_id <-NULL

dfTerritories <- data.frame(territories)
dfTerritories$X_id <-NULL





