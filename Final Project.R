library(jsonlite)
library(mongolite)
library(RCurl)
library(XML)
library(rvest)

# makes a dataframe of all the mvp candidates of all time by scraping them off basketball-reference.com
# no inputs
# output - dataframe of mvp candidates w/ stats
players <- function(){
  mvp1956URL <- getURL("http://www.basketball-reference.com/awards/awards_1956.html")
  playerStats <- readHTMLTable(mvp1956URL)[[1]]
  playerStats$MVP <- 0
  playerStats$Year <- 1956
  playerStats[1,]$MVP <- 1
  for(i in 1957:2016){
    URL <- getURL(paste("http://www.basketball-reference.com/awards/awards_",toString(i),".html", sep=""))
    Stats <- readHTMLTable(URL)[[1]]
    Stats$MVP <- 0
    Stats[1,]$MVP <- 1
    Stats$Year <- i
    playerStats <- rbind(playerStats,Stats)
  }
  playerStats <- playerStats[,-1]
  return (playerStats)
}

# DATA ACQUISITION / CLEANING

# if the data doesn't is already exist, load it.
if (!exists("MVP.Stats")){
  MVP.Stats <- players()
  # create random numbers for every row
  MVP.Stats$Set <- sample(1:nrow(MVP.Stats),nrow(MVP.Stats))
  # split the data into training and testing sets based on randum number
  MVP.Stats.Train <- MVP.Stats[MVP.Stats$Set<=nrow(MVP.Stats)/2,]
  MVP.Stats.Test <- MVP.Stats[MVP.Stats$Set>nrow(MVP.Stats)/2,]
  
  # export the MVP data as a csv
  write.csv(MVP.Stats, file = "MVP_STATS.csv")
  write.csv(MVP.Stats.Train, file = "MVP_Stats_Train.csv")
  write.csv(MVP.Stats.Test, file = "MVP_Stats_Test.csv")
}

# export the 2017 player data
if(!exists("newPlayers")){  
  # scrape the 2016-2017 player data and export as csv
  URL <- getURL("http://www.basketball-reference.com/leagues/NBA_2017_per_game.html")
  newPlayers <- readHTMLTable(URL)[[1]]
  colnames(newPlayers)[30] <- "PTS"
  write.csv(newPlayers, file = "newPlayers.csv")
}

# export the advanced 2017 player data
if(!exists("newPlayersAd")){
  # scrape the 2016-2017 advanced player data and export as csv
  URL <- getURL("http://www.basketball-reference.com/leagues/NBA_2017_advanced.html")
  newPlayersAd <- readHTMLTable(URL)[[1]]
  newPlayersAd <- newPlayersAd[,-c(20,25)]
  write.csv(newPlayersAd, file = "newPlayersAd.csv")
}

# DATA STORAGE WAS DONE MANUALLY THROUGH MONGOBOOSTERS USING THE EXPORTED CSV'S

# DATA RETRIEVAL / ADDITIONAL CLEANING

# import the MVP data
MVP.Stats <- mongolite::mongo(collection="MVP_STATS", db="MVP", url = "mongodb://localhost", verbose = FALSE, 
                                   options = mongolite::ssl_options())
MVP.Stats <- MVP.Stats$find()
MVP.Stats <- MVP.Stats[,-1]
MVP.Stats$`FT%` <- as.numeric(MVP.Stats$`FT%` * 10)
MVP.Stats$`FG%` <- as.numeric(MVP.Stats$`FG%` * 10)
MVP.Stats$G <- as.numeric(MVP.Stats$G)
MVP.Stats$PTS <- as.numeric(MVP.Stats$PTS)
MVP.Stats$TRB <- as.numeric(MVP.Stats$TRB)
MVP.Stats$WS <- as.numeric(MVP.Stats$WS)

# import the MVP test data
MVP.Stats.Test <- mongolite::mongo(collection="MVP_Stats_Test", db="MVP", url = "mongodb://localhost", verbose = FALSE, 
                              options = mongolite::ssl_options())
MVP.Stats.Test <- MVP.Stats.Test$find()
MVP.Stats.Test <- MVP.Stats.Test[,-1]
MVP.Stats.Test$`FT%` <- as.numeric(MVP.Stats.Test$`FT%` * 10)
MVP.Stats.Test$`FG%` <- as.numeric(MVP.Stats.Test$`FG%` * 10)
MVP.Stats.Test$MP <- as.numeric(MVP.Stats.Test$MP)
MVP.Stats.Test$AST <- as.numeric(MVP.Stats.Test$AST)
MVP.Stats.Test$TRB <- as.numeric(MVP.Stats.Test$TRB)
MVP.Stats.Test$WS <- as.numeric(MVP.Stats.Test$WS)

# import the MVP training data
MVP.Stats.Train <- mongolite::mongo(collection="MVP_Stats_Train", db="MVP", url = "mongodb://localhost", 
                                    verbose = FALSE, options = mongolite::ssl_options())
MVP.Stats.Train <- MVP.Stats.Train$find()
MVP.Stats.Train <- MVP.Stats.Train[,-1]
MVP.Stats.Train$`FT%` <- as.numeric(MVP.Stats.Train$`FT%` * 10)
MVP.Stats.Train$`FG%` <- as.numeric(MVP.Stats.Train$`FG%` * 10)
MVP.Stats.Train$MP <- as.numeric(MVP.Stats.Train$MP)
MVP.Stats.Train$AST <- as.numeric(MVP.Stats.Train$AST)
MVP.Stats.Train$TRB <- as.numeric(MVP.Stats.Train$TRB)
MVP.Stats.Train$WS <- as.numeric(MVP.Stats.Train$WS)

# import the 2016-2017 player data back into R from MongoDB
newPlayers <- mongolite::mongo(collection="newPlayers", db="MVP", url = "mongodb://localhost", 
                               verbose = FALSE, options = mongolite::ssl_options())
newPlayers <- newPlayers$find()
newPlayers <- newPlayers[,-1]
newPlayers <- newPlayers[!duplicated(newPlayers$Player),]
newPlayers$`FT%` <- as.numeric(newPlayers$`FT%`) * 10
removes <- as.numeric(newPlayers[is.na(newPlayers$`FT%`),]$Rk)[-1]
newPlayers <- na.omit(newPlayers)

# import the 2016-2017 advanced player data back into R from MongoDB
newPlayersAd <- mongolite::mongo(collection="newPlayersAd", db="MVP", url = "mongodb://localhost", 
                               verbose = FALSE, options = mongolite::ssl_options())
newPlayersAd <- newPlayersAd$find()
newPlayersAd <- newPlayersAd[,-1]
newPlayersAd <- newPlayersAd[!duplicated(newPlayersAd$Player),]
newPlayersAd$Rk <- as.numeric(newPlayersAd$Rk)
newPlayersAd <- newPlayersAd[!(newPlayersAd$Rk %in% removes),]
newPlayersAd$`TS%` <- as.numeric(newPlayersAd$`TS%`)
newPlayersAd <- na.omit(newPlayersAd)

# add WS to newPlayers & change newPlayers to numeric
newPlayers$WS <- as.numeric(newPlayersAd$WS)
newPlayers$WS[is.na(newPlayers$WS)] <- 0
newPlayers$G <- as.numeric(newPlayers$G)
newPlayers$PTS <- as.numeric(newPlayers$PTS)
newPlayers$TRB <- as.numeric(newPlayers$TRB)
newPlayers$MP <- as.numeric(newPlayers$MP)
newPlayers$AST <- as.numeric(newPlayers$AST)


# PREDICTIVE MODEL CONSTRUCTION #

# make a logistic regression model
model1 <- glm(formula = MVP ~ G + MP + PTS + Age + AST + TRB + WS + `FT%` + `FG%`, 
             data=MVP.Stats.Train, family=binomial, na.action = na.omit)
model2 <- glm(formula = MVP ~ MP + PTS + Age + AST + TRB + WS + `FT%` + `FG%`, 
              data=MVP.Stats.Train, family=binomial, na.action = na.omit)
model3 <- glm(formula = MVP ~ MP + PTS + AST + TRB + WS + `FT%` + `FG%`, 
              data=MVP.Stats.Train, family=binomial, na.action = na.omit)
model4 <- glm(formula = MVP ~ MP + AST + TRB + WS + `FT%` + `FG%`, 
              data=MVP.Stats.Train, family=binomial, na.action = na.omit)
model5 <- glm(formula = MVP ~ MP + AST + TRB + WS + `FT%`, 
              data=MVP.Stats.Train, family=binomial, na.action = na.omit)
# final model
model6 <- glm(formula = MVP ~ MP + TRB + WS + `FT%`, 
              data=MVP.Stats.Train, family=binomial, na.action = na.omit)

# results of the model
modelSum <- summary(model6)
modelSum

# test the model based on the test data set
predAcc <- function(){
  inAcc <- abs(MVP.Stats.Test$MVP - predict(model6, MVP.Stats.Test, type="response"))
  numRight <- length(which(inAcc<.5))
  total <- nrow(MVP.Stats.Test)
  return (numRight/total)
}

# although the prediction accuracy is high, the model is poor since it almost always misguesses
# the actual MVP's as not MVP's (false negative)
predAcc()

# MVP OF ALL TIME #

# determine the best season of all time, and determine the MVP
l <- nrow(MVP.Stats)
mvp <- "none"
mvpYear <- 0
mvpScore <- -100
MVP.Stats$score <- 0
for(i in 1:l){
  if(predict(model6,MVP.Stats[i,])>mvpScore){
    mvp <- MVP.Stats[i,]$Player
    mvpScore <- predict(model6,MVP.Stats[i,])
    mvpYear <- MVP.Stats[i,]$Year
  }
  MVP.Stats[i,]$score <- predict(model5,MVP.Stats[i,])
}

# mvp of all time
mvpAllTime <- paste(mvp,mvpYear,mvpScore,sep=", ")
mvpAllTime

# rank of all candidates for MVP of all time
MVPRankAllTime <- MVP.Stats
MVPRankAllTime <- MVPRankAllTime[with(MVPRankAllTime,order(-score)),]


# MVP OF 2016-2017 SEASON

# mvp of 2016 season
name <- ""
score <- -100
newPlayers$score <- 0
l <- nrow(newPlayers)
for(i in 1:l){
  if(predict(model6,newPlayers[i,])>score){
    score <- predict(model6,newPlayers[i,])
    name <- newPlayers[i,]$Player
  }
  newPlayers[i,]$score <- predict(model6,newPlayers[i,])
}

# rank of all candidates for MVP, candidates must play >20 minutes per game
MVPRank2017 <- newPlayers[newPlayers$MP>20,]
MVPRank2017 <- MVPRank2017[with(MVPRank2017,order(-score)),]
MVPRank2017$MVP.Chance <- predict(model6,MVPRank2017,type="response")

# export data for visualization
mvps <- MVPRank2017[,c(2,33)]
write.csv(mvps, file = "MVPs.csv")


#mvp
mvp2017 <- MVPRank2017[1,]$Player
mvp2017

# calculate confidence interval for chance of Rudy Gobert being MVP
inAcc <- abs(MVP.Stats.Test$MVP - predict(model5, MVP.Stats.Test, type="response"))
MAD <- mean(inAcc)
StDev <- 0.8 * MAD
StErr <- StDev / sqrt(nrow(MVP.Stats.Test)-1)
IntLow <- predict(model6,MVPRank2017[1,],type="response") - StErr * 1.96
IntHigh <- predict(model6,MVPRank2017[1,],type="response") + StErr * 1.96

# confidence interval
confInterval <- paste("(",toString(IntLow),", ",toString(IntHigh),")",sep="")