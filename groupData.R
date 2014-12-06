#Cody Hock
#This R script is building the training and tesing set's data
#to be used later on in other scripts.

#making MySQL connection
library(RMySQL)
con <- dbConnect(MySQL(), user='cody',password='',host='localhost',dbname='NFL')

#building the test model
stats <- "select * from stats where year < 2012"
scores<- "select * from scores where year < 2012"

#building the prediction model
predStats <- "select * from stats where year > 2011"
predScores<- "select * from scores where year > 2011"

#running queries
statsDF <- dbGetQuery(con,stats)
scoresDF <- dbGetQuery(con, scores)
scoreResult <-dbGetQuery(con, scores)

predStatsDF <- dbGetQuery(con,predStats)
predScoresDF <- dbGetQuery(con, predScores)


allData <-as.data.frame(NULL)
predAllData <-as.data.frame(NULL)
#scoresDF<-scoresDF[,-5]

#first renaming for the test model
scoresDF <- rename_columns(scoresDF)

#now renaming for the prediction model
predScoresDF <- rename_columns(predScoresDF)

#select scores.team1,stats.team from scores right join stats on scores.team1 = stats.team where stats.year = 2000 and scores.year = 2000;
scoresDF <- home_away_swap(scoresDF)
#now switching in the prediciton model
predScoresDF <- home_away_swap(predScoresDF)

#combining stats to teams at proprer locations
allData <- getGameData(scoresDF)
predAllData <- getGameData(predScoresDF)

#renaming columns
allData <- renameTeams(allData)
predAllData <-renameTeams(predAllData)

#appending the _A for away teams
appendAway(allData,predAllData)

#removing unused data
allData <-removeUnused(allData)
predAllData <-removeUnused(predAllData)

#appending the _H for home teams
appendHome(allData,predAllDAta)


save(predAllData,file = "~/Progs/R/NFL/predictionData.RData")
save(allData,file = "~/Progs/R/NFL/allData.RData")
dbDisconnect(con)


##############
#
#functions called from above
#
##############

#swapping stats of home and away teams to get into proper columns
home_away_swap <- function(df){
    for (i in 1:nrow(df)){
        #if the away team won flip it
        if(df[i,3] == '@'){
            yards <- df[i,7]
            to <- df[i,8]
            df[i,7] = df[i,9]
            df[i,8] = df[i,10]
            df[i,9] <- yards
            df[i,10] <- to
            remove(yards)
            remove(to)
            df[i,11] <- df[i,6]-df[i,5]
        }
        else{
            df[i,11] <- df[i,5]-df[i,6]
        }
    }
    return(df)
}

#after above swap, rename the columns
rename_columns <- function(df){
    colnames(df)[8] <- "HomeYards"
    colnames(df)[9] <- "HomeTO"
    colnames(df)[10] <- "AwayYards"
    colnames(df)[11] <- "AwayTO"
    df$Result <- 0
    df<-df[,-2]
    return(df)
}

getGameData <- function(scores){
    #function to return the data frame
    #that combines the stats of each team in the 
    #game into the correct vector
    #within the new data frame
    dataFrame <- as.data.frame(NULL)
    for(i in 1:nrow(scores)){
        scoreVector <- scores[i,]
        winVector <- dbGetQuery(con,paste(paste(paste(paste("select * from stats where team = '",scores[i,2],sep=""),"' and year =",sep=""), scores[i,1])," limit 1"))
        lossVector <- dbGetQuery(con,paste(paste(paste(paste("select * from stats where team = '",scores[i,4],sep=""),"' and year =",sep=""), scores[i,1])," limit 1"))
        if(length(winVector)>0 & length(lossVector)>0){
            #putting in order of: game stats, home stats, away stats - regaurdless of which team won
            if(scores[i,3] == '@'){
                res <- cbind(scoreVector,lossVector,winVector)
            }
            else{
                res <- cbind(scoreVector,winVector,lossVector)
            }
            dataFrame <- rbind(dataFrame,res)
        }
    }
    return(dataFrame)
}

#renames columns as Home/Away Team instead of Team 1/2
renameTeams <- function(dataFrame){
    colnames(dataFrame)[13] <- "Home Team"
    colnames(dataFrame)[45] <- "Away Team"
    return(dataFrame)
}

#appending "_A" for the columns coresponding to the away teams
appendAway <- function(trainDataFrame, testDataFrame){
    for (i in 45:75){
        colnames(trainDataFrame)[i] <- paste(colnames(trainDataFrame)[i],"_A",sep="")
        colnames(testDataFrame)[i] <- paste(colnames(testDataFrame)[i],"_A",sep="")
    }
}

#appending "_H" for the columns coresponding to the home teams
appendHome <- function(trainDataFrame, testDataFrame){
    for (i in 12:42){
        colnames(trainDataFrame)[i] <- paste(colnames(trainDataFrame)[i],"_H",sep="")
        colnames(testDataFrame)[i] <- paste(colnames(testDataFrame)[i],"_H",sep="")
    }
}

#removing unused data from the data frame 
removeUnused <- function(dataFrame){
    dataFrame <- dataFrame[,-44]
    dataFrame <- dataFrame[,-12]
    return(dataFrame)
}
