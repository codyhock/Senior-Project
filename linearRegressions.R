load("~/Progs/R/NFL/allData.RData")
load("~/Progs/R/NFL/predictionData.RData")

#function call to calculate the turnovers
#did once, don't need to again
if(FALSE){
    add_turnovers(allData)
    add_turnovers(predAllData)
}
#Home Team wins 57% of the time

####################
#
#Turnover ratio
#
####################

TO_model <-
    lm(Result ~
             HomeOffTO:Pts_H+
             AwayOffTO:Pts_A+
             HomeOffTO:Pts_A+
             AwayOffTO:Pts_H+
           HomeOffTO+
           AwayOffTO+
             DefInt_H+
             DefInt_A+
           DefInt_H:Pts_H+
           DefInt_A:Pts_A+
             TODifference:PassInt_H+
             TODifference:PassInt_A+
           TODifference:Fmb_H+
           TODifference:Fmb_A+
             TODifference:DefInt_H+
             TODifference:DefInt_A+
           PassInt_H:DefInt_A+
           PassInt_A:DefInt_H+
             DefInt_H:AwayOffTO+
             DefInt_H:HomeOffTO+
           Fmb_H:HomeOffTO+
           Fmb_A:AwayOffTO,    
      data = allData)

sink("~/Progs/R/NFL/TO_Model.txt")
summary(TO_model)
sink()

TO_pred <- predict(TO_model, predAllData)

save_plot(predAllData, TO_pred, '~/Progs/R/NFL/TurnoverPrediction.jpg')

sink("~/Progs/R/NFL/TurnoverPrediction.txt")
summary(TO_pred)
paste("R-squared: ",R_squared(predAllData,TO_pred))
paste(paste("Accuracy : ",prediction_accuracy(predAllData,TO_pred)),"%")
sink()


####################
#
#Defensive stats
#
####################

Def_model <-
  lm(Result ~
       RushYG_H:DefRushYG_A+
       RushYG_A:DefRushYG_H+
         RushYA_H:DefRushYA_A+
         RushYA_A:DefRushYA_H+
       PassYG_H:DefPassYG_A+
       PassYG_A:DefPassYG_H+
         PassInt_H:DefInt_A+
         PassInt_A:DefInt_H+
       PassTD_H:DefPassTD_A+
       PassTD_A:DefPassTD_H+
         RushTD_H:DefRushTD_A+
         RushTD_A:DefRushTD_H+
       PtsAllowed_H+
       PtsAllowed_A+
         DefRushYG_H:Pts_H+
         DefRushYG_A:Pts_A+
       DefRushYds_A+
       DefRushYds_H,
    data = allData)

sink("~/Progs/R/NFL/Def_Model.txt")
summary(Def_model)
sink()

Def_pred <- predict(Def_model, predAllData)

save_plot(predAllData, Def_pred, '~/Progs/R/NFL/DefensivePrediction.jpg')

sink("~/Progs/R/NFL/DefensivePrediction.txt")
summary(Def_pred)
paste("R-squared: ",R_squared(predAllData,Def_pred))
paste(paste("Accuracy : ",prediction_accuracy(predAllData,Def_pred)),"%")
sink()


####################
#
#Quaterback stats
#
####################

QB_model <-
    lm(Result ~
           PassCmp_H:DefPassCmp_A+
           PassCmp_A:DefPassCmp_H+
             PassTD_H:RushTD_H+
             PassTD_A:RushTD_A+
          # PassTD_H:PassInt_H+
          # PassTD_A:PassInt_A+
            # PassYG_H:DefPassYG_A+
            # PassYG_A:DefPassYG_H+
           DefQBRating_H+
           DefQBRating_A+
             DefQBRating_H:PtsAllowed_H+
             DefQBRating_A:PtsAllowed_A+
           DefQBRating_H:Pts_H+
           DefQBRating_A:Pts_A+
             PassTD_H:Pts_H+
             PassTD_A:Pts_A,
         data = allData)

sink("~/Progs/R/NFL/QB_Model.txt")
summary(QB_model)
sink()

QB_pred <- predict(QB_model, predAllData)

save_plot(predAllData, QB_pred, '~/Progs/R/NFL/QBPrediction.jpg')

sink("~/Progs/R/NFL/QBPrediction.txt")
summary(QB_pred)
paste("R-squared: ",R_squared(predAllData,QB_pred))
paste(paste("Accuracy : ",prediction_accuracy(predAllData,QB_pred)),"%")
sink()


####################
#
#Rushing stats
#
####################

Rush_model <-
    lm(Result ~
           RushTD_H:DefRushTD_A+
           RushTD_A:DefRushTD_H+
             RushYG_H:DefRushYG_A+
             RushYG_A:DefRushYG_H+
           RushYA_H:DefRushYA_A+
           RushYA_A:DefRushYA_H+
             RushYds_H+
             RushYds_A+
           RushYA_H:RushTD_H+
           RushYA_A:RushTD_A+
             RushYG_H:Pts_H+
             RushYG_A:Pts_A+
          # RushYG_H:PassYG_H+
          # RushYG_A:PassYG_A+
             RushYA_H:PassCmp_H+
             RushYA_A:PassCmp_A+
           RushTD_H:PtsAllowed_A+
           RushTD_A:PtsAllowed_H+
             Fmb_H:HomeOffTO+
             Fmb_A:AwayOffTO,
         data = allData)

sink("~/Progs/R/NFL/Rush_Model.txt")
summary(Rush_model)
sink()

Rush_pred <- predict(Rush_model, predAllData)

save_plot(predAllData, Rush_pred, '~/Progs/R/NFL/RushingPrediction.jpg')

sink("~/Progs/R/NFL/RushingPrediction.txt")
summary(Rush_pred)
paste("R-squared: ",R_squared(predAllData,Rush_pred))
paste(paste("Accuracy : ",prediction_accuracy(predAllData,Rush_pred)),"%")
sink()

####################
#
#Field Goal stats
#
####################
FG_model <-
    lm(Result ~
           FGM_H+
           FGM_A+
           FGM_H:Pts_H+
           FGM_A:Pts_A,
           data = allData)

sink("~/Progs/R/NFL/FG_Model.txt")
summary(FG_model)
sink()

FG_pred <- predict(FG_model, predAllData)

save_plot(predAllData, FG_pred, '~/Progs/R/NFL/FGPrediction.jpg')

sink("~/Progs/R/NFL/FGPrediction.txt")
summary(FG_pred)
paste("R-squared: ",R_squared(predAllData,FG_pred))
paste(paste("Accuracy : ",prediction_accuracy(predAllData,FG_pred)),"%")
sink()

####################
#
#Points
#
####################

#adding PPG stats to the data frames
#already done, do not need to do again
if(FALSE){
    calc_points(allData)
    calc_points(predAllData)
}

PPG_model <-
    lm(Result ~
           PPG_H+
           PPG_A+
           DefPPG_H+
           DefPPG_A+
           PPG_H:DefPPG_H+
           PPG_A:DefPPG_A
           #PPG_H:DefPPG_A+
           #PPG_A:DefPPG_H
          # DifPPG_H+
           #DifPPG_A+
          # DiffPPG
           ,
           data = allData)

sink("~/Progs/R/NFL/PPG_Model.txt")
summary(PPG_model)
sink()

PPG_pred <- predict(PPG_model, predAllData)

save_plot(predAllData, PPG_pred, '~/Progs/R/NFL/PPGPrediction.jpg')

sink("~/Progs/R/NFL/PPGPrediction.txt")
summary(PPG_pred)
paste("R-squared: ",R_squared(predAllData,PPG_pred))
paste(paste("Accuracy : ",prediction_accuracy(predAllData,PPG_pred)),"%")
sink()

####################
#
#Combining the models
#
####################


Large_model <-
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
       data = allData)

sink("~/Progs/R/NFL/Large_Model.txt")
summary(Large_model)
sink()

Large_pred <- predict(Large_model, predAllData)

save_plot(predAllData, Large_pred, '~/Progs/R/NFL/LargePrediction.jpg')

sink("~/Progs/R/NFL/LargePrediction.txt")
summary(Large_pred)
paste("Teams had the same record: %",round(sameRecord/nrow(predAllData)*100,2),sep="")
paste("Better record team won:  %",round(betterTeam/nrow(predAllData)*100,2),sep="")
paste(paste("My Accuracy : ",prediction_accuracy(predAllData,Large_pred)),"%")
paste("R-squared: ",R_squared(predAllData,Large_pred))
sink()




betterTeam <- 0
sameRecord <- 0
for(i in 1:nrow(predAllData)){
    # teams had the same record
    if(predAllData[i,"W_H"] == predAllData[i,"W_A"]){
        sameRecord <- sameRecord + 1
    }
    # Home Team had the better record and won
    else if((predAllData[i,"W_H"] > predAllData[i,"W_A"]) && (predAllData[i,"Result"] > 0)){
        betterTeam <- betterTeam + 1
    }
    # Away Team had the better record and won
    else if((predAllData[i,"W_H"] < predAllData[i,"W_A"]) && (predAllData[i,"Result"] < 0)){
        betterTeam <- betterTeam + 1
    }
}
343/532
63/532
#############################
#
#The functions to be called from above, 
#adding to data frames 
#returning prediction accuracy
#saving the plots
#
#############################
add_turnovers <- function(df){
    #acreating null columns to be filled in
    allData$TODifference <- 0
    predAllData$TODifference <- 0
    allData$HomeOrAway <- 0
    predAllData$HomeOrAway <- 0
    allData$HomeOffTO <- 0
    predAllData$HomeOffTO <- 0
    allData$AwayOffTO <- 0
    predAllData$AwayOffTO <- 0
    
    #Setting turnover difference depending on which team won
    #and setting a valuue representing which team won
    for (i in 1:nrow(allData)){
        #home team won
        if(allData[i,11] > 0){
            allData[i,74] <- allData[i,10] - allData[i,8] 
            allData[i,"HomeOrAway"] <- 1
        }
        else{
            allData[i,74] <- allData[i,8] - allData[i,10]
            allData[i,"HomeOrAway"] <- -1
        }
    }
    #and the same for the prediction data frame
    for (i in 1:nrow(predAllData)){
        #home team won
        if(predAllData[i,11] > 0){
            predAllData[i,74] <- predAllData[i,10] - predAllData[i,8]  
            predAllData[i,"HomeOrAway"] <- 1
        }
        else{
            predAllData[i,74] <- predAllData[i,8] - predAllData[i,10]
            predAllData[i,"HomeOrAway"] <- -1
        }
    }

    #getting the total turnovers committed by each team
    for(i in 1:nrow(allData)){
        allData[,"HomeOffTO"] <- allData[,"Fmb_H"] + allData[,"PassInt_H"]
        allData[,"AwayOffTO"] <- allData[,"Fmb_A"] + allData[,"PassInt_A"]
    }
    #and now for prediction data frame
    for(i in 1:nrow(predAllData)){
        predAllData[,"HomeOffTO"] <- predAllData[,"Fmb_H"] + predAllData[,"PassInt_H"]
        predAllData[,"AwayOffTO"] <- predAllData[,"Fmb_A"] + predAllData[,"PassInt_A"]
    }
      
    save(predAllData,file = "~/Progs/R/NFL/predictionData.RData")
    save(allData,file = "~/Progs/R/NFL/allData.RData")
}

calc_points <- function(df){
    #creating null columns to be filled in
    allData$PPG_H <- 0
    predAllData$PPG_H <- 0
    allData$DefPPG_H <- 0
    predAllData$DefPPG_H <- 0
    allData$PPG_A <- 0
    predAllData$PPG_A <- 0
    allData$DefPPG_A <- 0
    predAllData$DefPPG_A <- 0
    allData$DifPPG_H <-0
    allData$DifPPG_A <-0
    allData$DiffPPG <-0
    predAllData$DifPPG_H <-0
    predAllData$DifPPG_A <-0
    predAllData$DiffPPG <-0
    
    #adding total games played to divide the total points by to get the points per game stat
    for(i in 1:nrow(allData)){
        games <- (allData[i,"W_H"]+allData[i,"L_H"]+allData[i,"T_H"])
        allData[i,"PPG_H"] <- allData[i,"Pts_H"]/games
        allData[i,"DefPPG_H"]<- allData[i,"PtsAllowed_H"]/games
        
        games <- (allData[i,"W_A"]+allData[i,"L_A"]+allData[i,"T_A"])
        allData[i,"PPG_A"] <- allData[i,"Pts_A"]/games
        allData[i,"DefPPG_A"]<- allData[i,"PtsAllowed_A"]/games
    }
    #and now for the prediction data frame
    for(i in 1:nrow(predAllData)){
        games <- (predAllData[i,"W_H"]+predAllData[i,"L_H"]+predAllData[i,"T_H"])
        predAllData[i,"PPG_H"] <- predAllData[i,"Pts_H"]/games
        predAllData[i,"DefPPG_H"]<- predAllData[i,"PtsAllowed_H"]/games
        
        games <- (predAllData[i,"W_A"]+predAllData[i,"L_A"]+predAllData[i,"T_A"])
        predAllData[i,"PPG_A"] <- predAllData[i,"Pts_A"]/games
        predAllData[i,"DefPPG_A"]<- predAllData[i,"PtsAllowed_A"]/games
    }

    #calculating the differnce of the PPG of each teams
    for(i in 1:nrow(allData)){
        allData[i,"DifPPG_H"] <- allData[i,"PPG_H"] - allData[i,"DefPPG_H"]
        allData[i,"DifPPG_A"] <- allData[i,"PPG_A"] - allData[i,"DefPPG_A"]
        allData[i,"DifPPG"] <- allData[i,"DifPPG_H"] - allData[i,"DifPPG_A"]
    }
    #and now for the prediction data frame
    for(i in 1:nrow(predAllData)){
        predAllData[i,"DifPPG_H"] <- predAllData[i,"PPG_H"] - predAllData[i,"DefPPG_H"]
        predAllData[i,"DifPPG_A"] <- predAllData[i,"PPG_A"] - predAllData[i,"DefPPG_A"]
        predAllData[i,"DifPPG"] <- predAllData[i,"DifPPG_H"] - predAllData[i,"DifPPG_A"]
    }
    
    save(predAllData,file = "~/Progs/R/NFL/predictionData.RData")
    save(allData,file = "~/Progs/R/NFL/allData.RData")
}

prediction_accuracy <- function(df, predicion){
    correct <- 0
    for (i in 1:nrow(df)){
        if(df[i,"Result"] < 0 && predicion[i] < 0){
            correct <- correct + 1
        }
        else if(df[i,"Result"] > 0 && predicion[i] > 0){
            correct <- correct + 1
        }
    }
    return (round(correct / length(predicion),4) * 100)
    
}

save_plot <- function(df, prediction, fileName){
    f <- fileName
    jpeg(f)
    plot(prediction,df$Result,main="Score Differential",ylab="Actual",xlab="Predicted",xlim=c(-60,60),ylim=c(-60,60))
    abline(0,1,col="red")
    abline(v=0,col="blue")
    abline(h=0,col="blue")
    dev.off()
}

R_squared <- function(df, prediction){
    return (round(cor(df[,"Result"],prediction)^2,4))
}