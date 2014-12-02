load("~/Progs/R/NFL/allData.RData")
load("~/Progs/R/NFL/predictionData.RData")

newData <- rbind(allData, predAllData)

#building the test model
stats <- "select * from stats where year = 2014"
scores<- "select * from scores where year = 2014"

statsDF <- dbGetQuery(con,stats)
scoresDF <- dbGetQuery(con, scores)

#first renaming for the test model
scoresDF <- rename_columns(scoresDF)

#select scores.team1,stats.team from scores right join stats on scores.team1 = stats.team where stats.year = 2000 and scores.year = 2000;
scoresDF <- home_away_swap(scoresDF)

newPredData <-as.data.frame(NULL)

for(i in 1:nrow(scoresDF)){
    scoreVector <- scoresDF[i,]
    winVector <- dbGetQuery(con,paste(paste(paste(paste("select * from stats where team = '",scoresDF[i,2],sep=""),"' and year =",sep=""), scoresDF[i,1])," limit 1"))
    lossVector <- dbGetQuery(con,paste(paste(paste(paste("select * from stats where team = '",scoresDF[i,4],sep=""),"' and year =",sep=""), scoresDF[i,1])," limit 1"))
    if(length(winVector)>0 & length(lossVector)>0){
        if(scoresDF[i,3] == '@'){
            res <- cbind(scoreVector,lossVector,winVector)
        }
        else{
            res <- cbind(scoreVector,winVector,lossVector)
        }
        newPredData <- rbind(newPredData,res)
        
    }
}
colnames(newPredData)[13] <- "Home Team"
colnames(newPredData)[45] <- "Away Team"

for (i in 45:75){
    colnames(newPredData)[i] <- paste(colnames(newPredData)[i],"_A",sep="")    
}

newPredData <- newPredData[,-44]
newPredData <- newPredData[,-12]

for (i in 12:42){
    colnames(newPredData)[i] <- paste(colnames(newPredData)[i],"_H",sep="")    
}

newPredData$TODifference <- 0
newPredData$HomeOrAway <- 0
newPredData$HomeOffTO <- 0
newPredData$AwayOffTO <- 0
newPredData$PPG_H <- 0
newPredData$DefPPG_H <- 0
newPredData$PPG_A <- 0
newPredData$DefPPG_A <- 0
newPredData$DifPPG_H <- 0
newPredData$DifPPG_A <- 0
newPredData$DifPPG <- 0
newPredData$DifPPG_H <- 0

#making my 2014 data frame have the same dimensions as the model data frame
for(i in 1:nrow(newPredData)){
    #TO difference
    newPredData[i,"TODifference"] <- newPredData[i,"AwayTO"] - newPredData[i,"HomeTO"]
    
    #who won
    newPredData[i,"HomeOrAway"] <- newPredData[i,"Result"] / abs(newPredData[i,"Result"])
    
    #Offensive turnovers
    newPredData[i,"HomeOffTO"] <- newPredData[i,"Fmb_H"] + newPredData[i,"PassInt_H"]
    newPredData[i,"AwayOffTO"] <- newPredData[i,"Fmb_A"] + newPredData[i,"PassInt_A"]
    
    #adding total games played to divide the total points by to get the points per game stat
    games <- (newPredData[i,"W_H"]+newPredData[i,"L_H"]+newPredData[i,"T_H"])
    newPredData[i,"PPG_H"] <- newPredData[i,"Pts_H"]/games
    newPredData[i,"DefPPG_H"]<- newPredData[i,"PtsAllowed_H"]/games
        
    games <- (newPredData[i,"W_A"]+newPredData[i,"L_A"]+newPredData[i,"T_A"])
    newPredData[i,"PPG_A"] <- newPredData[i,"Pts_A"]/games
    newPredData[i,"DefPPG_A"]<- newPredData[i,"PtsAllowed_A"]/games
    
    #calculating the differnce of the PPG of each teams
    newPredData[i,"DifPPG_H"] <- newPredData[i,"PPG_H"] - newPredData[i,"DefPPG_H"]
    newPredData[i,"DifPPG_A"] <- newPredData[i,"PPG_A"] - newPredData[i,"DefPPG_A"]
    newPredData[i,"DifPPG"] <- newPredData[i,"DifPPG_H"] - newPredData[i,"DifPPG_A"]
    
}
newData$DiffPPG <- NULL
newPredData$DiffPPG <- NULL
###################
#
#now for next weeks games
#
###################
nextScores <- "select * from scores where year = 2014 and week = 14;"
nextScoresDF <- dbGetQuery(con, nextScores)
nextScoresDF <- rename_columns(nextScoresDF)
#don't need home/away swap bc they are all nulled out
nextWeekData <-as.data.frame(NULL)
for(i in 1:nrow(nextScoresDF)){
    scoreVector <- nextScoresDF[i,]
    winVector <- dbGetQuery(con,paste(paste(paste(paste("select * from stats where team = '",nextScoresDF[i,2],sep=""),"' and year =",sep=""), nextScoresDF[i,1])," limit 1"))
    lossVector <- dbGetQuery(con,paste(paste(paste(paste("select * from stats where team = '",nextScoresDF[i,4],sep=""),"' and year =",sep=""), nextScoresDF[i,1])," limit 1"))
    if(length(winVector)>0 & length(lossVector)>0){
        if(nextScoresDF[i,3] == '@'){
            res <- cbind(scoreVector,lossVector,winVector)
        }
        else{
            res <- cbind(scoreVector,winVector,lossVector)
        }
        nextWeekData <- rbind(nextWeekData,res)
        
    }
}
colnames(nextWeekData)[13] <- "Home Team"
colnames(nextWeekData)[45] <- "Away Team"

for (i in 45:75){
    colnames(nextWeekData)[i] <- paste(colnames(nextWeekData)[i],"_A",sep="")    
}

nextWeekData <- nextWeekData[,-44]
nextWeekData <- nextWeekData[,-12]

for (i in 12:42){
    colnames(nextWeekData)[i] <- paste(colnames(nextWeekData)[i],"_H",sep="")    
}

nextWeekData$TODifference <- 0
nextWeekData$HomeOrAway <- 0
nextWeekData$HomeOffTO <- 0
nextWeekData$AwayOffTO <- 0
nextWeekData$PPG_H <- 0
nextWeekData$DefPPG_H <- 0
nextWeekData$PPG_A <- 0
nextWeekData$DefPPG_A <- 0
nextWeekData$DifPPG_H <- 0
nextWeekData$DifPPG_A <- 0
nextWeekData$DifPPG <- 0
nextWeekData$DifPPG_H <- 0

#making my 2014 data frame have the same dimensions as the model data frame
for(i in 1:nrow(nextWeekData)){
    #TO difference
    nextWeekData[i,"TODifference"] <- nextWeekData[i,"AwayTO"] - nextWeekData[i,"HomeTO"]
    
    #who won
    #divide by 0
    #nextWeekData[i,"HomeOrAway"] <- nextWeekData[i,"Result"] / abs(nextWeekData[i,"Result"])
    
    #Offensive turnovers
    nextWeekData[i,"HomeOffTO"] <- nextWeekData[i,"Fmb_H"] + nextWeekData[i,"PassInt_H"]
    nextWeekData[i,"AwayOffTO"] <- nextWeekData[i,"Fmb_A"] + nextWeekData[i,"PassInt_A"]
    
    #adding total games played to divide the total points by to get the points per game stat
    games <- (nextWeekData[i,"W_H"]+nextWeekData[i,"L_H"]+nextWeekData[i,"T_H"])
    nextWeekData[i,"PPG_H"] <- nextWeekData[i,"Pts_H"]/games
    nextWeekData[i,"DefPPG_H"]<- nextWeekData[i,"PtsAllowed_H"]/games
    
    games <- (nextWeekData[i,"W_A"]+nextWeekData[i,"L_A"]+nextWeekData[i,"T_A"])
    nextWeekData[i,"PPG_A"] <- nextWeekData[i,"Pts_A"]/games
    nextWeekData[i,"DefPPG_A"]<- nextWeekData[i,"PtsAllowed_A"]/games
    
    #calculating the differnce of the PPG of each teams
    nextWeekData[i,"DifPPG_H"] <- nextWeekData[i,"PPG_H"] - nextWeekData[i,"DefPPG_H"]
    nextWeekData[i,"DifPPG_A"] <- nextWeekData[i,"PPG_A"] - nextWeekData[i,"DefPPG_A"]
    nextWeekData[i,"DifPPG"] <- nextWeekData[i,"DifPPG_H"] - nextWeekData[i,"DifPPG_A"]
    
}

##################
#
#Now predicting with the most accurate linear regression from earlier
#
##################

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
       data = newData)

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



round(1.4)
nextWeek <- lapply(nextWeekpred,round)
?lapply












