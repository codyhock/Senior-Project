#Cody Hock
#This R script is for the games being played during week 14 and 15 of the 2014 NFL season.
#The code here makes a great use of code in other R scripts and it is to show
#the progress that has been made in this project.

#loading the large data frames that were used in the training and testing sets earlier
load("~/Progs/R/NFL/allData.RData")
load("~/Progs/R/NFL/predictionData.RData")

#I am combinding all of the data from 2000-2013 to be used as a training set
#for this weeks games
newData <- rbind(allData, predAllData)

#connecting to the MySQL database to get the new data
library(RMySQL)
con <- dbConnect(MySQL(), user='cody',password='',host='localhost',dbname='NFL')

#building the test model
stats <- "select * from stats where year = 2014"
scores<- "select * from scores where year = 2014"

#running queries
statsDF <- dbGetQuery(con,stats)
scoresDF <- dbGetQuery(con, scores)

#first renaming for the test model, this function is in groupData.R
scoresDF <- rename_columns(scoresDF)

#getting in order of winning team for the individual games, this function is in groupData.R
scoresDF <- home_away_swap(scoresDF)

#getting the data frame with all of the stats for the teams of each game, this function is in groupData.R
newPredData <- getGameData(scoresDF)

#renaming teams columns, this function is in groupData.R
newPredData <-renameTeams(newPredData)

#this method varies from the one in groupData.R because there is only 1 data frame to edit
newPredData <-appendAwayWeek(newPredData)

#removing unused data, this funciton is in groupData.R
newPredData <-removeUnused(newPredData)

#this method varies from the one in groupData.R because there is only 1 data frame to edit
newPredData <-appendHomeWeek(newPredData)

#adding columns to match dimensions of the data frames
newPredData <-addColumns(newPredData)

#making my 2014 data frame have the same dimensions as the model data frame
newPredData <-calculateColumns(newPredData, 1)

##########################################################
#
#now for next weeks games
#
##########################################################
nextScores <- "select * from scores where year = 2014 and week = 14;"
nextScoresDF <- dbGetQuery(con, nextScores)
nextScoresDF <- rename_columns(nextScoresDF)

#don't need home/away swap bc they are all nulled out

#getting the data frame with all of the stats for the teams of each game, this function is in groupData.R
nextWeekData <- getGameData(nextScoresDF)

#renaming teams columns, this function is in groupData.R
nextWeekData <-renameTeams(nextWeekData)

#this method varies from the one in groupData.R because there is only 1 data frame to edit
nextWeekData <-appendAwayWeek(nextWeekData)

#removing unused data, this funciton is in groupData.R
nextWeekData <-removeUnused(nextWeekData)

#this method varies from the one in groupData.R because there is only 1 data frame to edit
nextWeekData <-appendHomeWeek(nextWeekData)

#adding columns to match dimensions of the data frames
nextWeekData <-addColumns(nextWeekData)

#making my 2014 data frame have the same dimensions as the model data frame
nextWeekData <-calculateColumns(nextWeekData, 0)

########################################################
#
#Now predicting with the most accurate linear regression from earlier
#
########################################################

#making sure the TO margin is correct
newData2 <- newData
newData2$TODifference <- (newData2$DefInt_H - newData2$PassInt_H)-(newData2$DefInt_A - newData2$PassInt_A)

model <-
    lm(Result ~
           PPG_H:DefPPG_A+
           PPG_A:DefPPG_H+
           TODifference:PassInt_H+
           TODifference:PassInt_A+
           DefInt_H:TODifference+
           DefInt_A:TODifference+
           DefRushYG_H:Pts_H+
           DefRushYG_A:Pts_A+
           DefQBRating_H:DefPPG_H+
           DefQBRating_A:DefPPG_A+
           DefQBRating_H:PPG_H+
           DefQBRating_A:PPG_A+
           RushYds_H+
           RushYds_A+
           RushTD_H:RushYA_H+
           RushTD_A:RushYA_A+
           RushYG_H:PPG_H+
           RushYG_A:PPG_A+
           RushTD_H:DefPPG_A+
           RushTD_A:DefPPG_H
       ,
       data = newData2)

thisYearpred <- predict(model, newPredData)
nextWeekpred <- predict(model, nextWeekData)

wins <- 0
for(i in 1:nrow(newPredData)){
    if(newPredData[i,"Result"] / abs(newPredData[i,"Result"] ) == thisYearpred[i]/abs(thisYearpred[i])){
        wins <- wins + 1
    }
}
nrow(newPredData)
sink("~/Progs/R/NFL/2014/2014Predictions.txt")
paste("Thru week 13 (ended Monday, December 1st)")
paste("Total games:",nrow(newPredData))
paste("Correctly picked:",wins)
paste(paste(paste("Record: ",wins),"-"),nrow(newPredData)-wins)
paste(paste("Accuracy:",round(100*wins/nrow(newPredData),2)),"%")
sink()


sink("~/Progs/R/NFL/2014/2014Week14.txt")
nextWeek
sink()

nextWeek <- lapply(nextWeekpred,round)


###################################################
#
#funcitons
#
###################################################
#functions to append _A and _H to correct columns repectively
appendAwayWeek <- function(dataFrame){
    for (i in 45:75){
        colnames(dataFrame)[i] <- paste(colnames(dataFrame)[i],"_A",sep="")    
    }
    return(dataFrame)
}

appendHomeWeek <- function(dataFrame){
    for (i in 12:42){
        colnames(dataFrame)[i] <- paste(colnames(dataFrame)[i],"_H",sep="")    
    }
    return(dataFrame)
}

#this function adds the necessary columns that the previous larger dataFrame has
#this is necessary because the dimensions of the data frame used to model must
#match exactly with the data frame used to test
addColumns <- function(dataFrame){
    dataFrame$TODifference <- 0
    dataFrame$HomeOrAway <- 0
    dataFrame$HomeOffTO <- 0
    dataFrame$AwayOffTO <- 0
    dataFrame$PPG_H <- 0
    dataFrame$DefPPG_H <- 0
    dataFrame$PPG_A <- 0
    dataFrame$DefPPG_A <- 0
    dataFrame$DifPPG_H <- 0
    dataFrame$DifPPG_A <- 0
    dataFrame$DifPPG <- 0
    return(dataFrame)
}

#calculating average points scored as well as total turnovers
calculateColumns <- function(dataFrame, res){
    for(i in 1:nrow(dataFrame)){
        #who won
        if(res > 0){
            dataFrame[i,"HomeOrAway"] <- dataFrame[i,"Result"] / abs(dataFrame[i,"Result"])
        }
        
        #Offensive turnovers
        dataFrame[i,"HomeOffTO"] <- dataFrame[i,"Fmb_H"] + dataFrame[i,"PassInt_H"]
        dataFrame[i,"AwayOffTO"] <- dataFrame[i,"Fmb_A"] + dataFrame[i,"PassInt_A"]
        
        #turnover difference
        dataFrame[i,"TODifference"] <- dataFrame[i,"HomeTO"] - dataFrame[i,"AwayTO"]

        #adding total games played to divide the total points by to get the points per game stat
        games <- (dataFrame[i,"W_H"]+dataFrame[i,"L_H"]+dataFrame[i,"T_H"])
        dataFrame[i,"PPG_H"] <- dataFrame[i,"Pts_H"]/games
        dataFrame[i,"DefPPG_H"]<- dataFrame[i,"PtsAllowed_H"]/games
        
        games <- (dataFrame[i,"W_A"]+dataFrame[i,"L_A"]+dataFrame[i,"T_A"])
        dataFrame[i,"PPG_A"] <- dataFrame[i,"Pts_A"]/games
        dataFrame[i,"DefPPG_A"]<- dataFrame[i,"PtsAllowed_A"]/games
        
        #calculating the differnce of the PPG of each teams
        dataFrame[i,"DifPPG_H"] <- dataFrame[i,"PPG_H"] - dataFrame[i,"DefPPG_H"]
        dataFrame[i,"DifPPG_A"] <- dataFrame[i,"PPG_A"] - dataFrame[i,"DefPPG_A"]
        dataFrame[i,"DifPPG"] <- dataFrame[i,"DifPPG_H"] - dataFrame[i,"DifPPG_A"]
    }
    return(dataFrame)
}