library(RMySQL)

con <- dbConnect(MySQL(), user='cody',password='',host='localhost',dbname='NFL')

#building the test model
stats <- "select * from stats where year < 2012"
scores<- "select * from scores where year < 2012"

#building the prediction model
predStats <- "select * from stats where year > 2011"
predScores<- "select * from scores where year > 2011"

statsDF <- dbGetQuery(con,stats)
scoresDF <- dbGetQuery(con, scores)
scoreResult <-dbGetQuery(con, scores)

predStatsDF <- dbGetQuery(con,predStats)
predScoresDF <- dbGetQuery(con, predScores)

nrow(statsDF)
nrow(scoresDF)

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



##########
#
#combining at proprer locations
#
###########

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
        allData <- rbind(allData,res)
        
    }
}

for(i in 1:nrow(predScoresDF)){
    scoreVector <- predScoresDF[i,]
    winVector <- dbGetQuery(con,paste(paste(paste(paste("select * from stats where team = '",predScoresDF[i,2],sep=""),"' and year =",sep=""), predScoresDF[i,1])," limit 1"))
    lossVector <- dbGetQuery(con,paste(paste(paste(paste("select * from stats where team = '",predScoresDF[i,4],sep=""),"' and year =",sep=""), predScoresDF[i,1])," limit 1"))
    if(length(winVector)>0 & length(lossVector)>0){
        if(predScoresDF[i,3] == '@'){
            res <- cbind(scoreVector,lossVector,winVector)
        }
        else{
            res <- cbind(scoreVector,winVector,lossVector)
        }
        predAllData <- rbind(predAllData,res)
        
    }
}
colnames(allData)[13] <- "Home Team"
colnames(allData)[45] <- "Away Team"

colnames(predAllData)[13] <- "Home Team"
colnames(predAllData)[45] <- "Away Team"

for (i in 45:75){
    colnames(allData)[i] <- paste(colnames(allData)[i],"_A",sep="")
    colnames(predAllData)[i] <- paste(colnames(predAllData)[i],"_A",sep="")
    
}
allData <- allData[,-44]
predAllData <- predAllData[,-44]
allData <- allData[,-12]
predAllData <- predAllData[,-12]

for (i in 12:42){
    colnames(allData)[i] <- paste(colnames(allData)[i],"_H",sep="")
    colnames(predAllData)[i] <- paste(colnames(predAllData)[i],"_H",sep="")
    
}
#colnames(predAllData)[42] <- paste(colnames(predAllData)[42],"_W",sep="")

save(predAllData,file = "~/Progs/R/NFL/predictionData.RData")
save(allData,file = "~/Progs/R/NFL/allData.RData")
dbDisconnect(con)



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

rename_columns <- function(df){
    colnames(df)[8] <- "HomeYards"
    colnames(df)[9] <- "HomeTO"
    colnames(df)[10] <- "AwayYards"
    colnames(df)[11] <- "AwayTO"
    df$Result <- 0
    df<-df[,-2]
    return(df)
}
sessionInfo()
