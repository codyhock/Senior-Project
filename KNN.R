#########################
#
#Getting data for the KNN
#
#########################
load("~/Progs/R/NFL/allData.RData")
load("~/Progs/R/NFL/predictionData.RData")

#the reason I am only getting certain data here is because these are the 
#stats I found to be most significant in my linear regressions
knn_allData <- allData[,
                    c("Result",
                      "W_H",
                      "W_A",
                      "RushYds_H",
                      "RushTD_H",
                      "RushYA_H",
                      "RushYG_H",
                      "PassInt_H",
                      "DefRushYG_H",
                      "DefInt_H",
                      "DefQBRating_H",
                      "RushYds_A",
                      "RushTD_A",
                      "RushYA_A",
                      "RushYG_A",
                      "PassInt_A",
                      "DefRushTD_A",
                      "DefRushYG_A",
                      "DefInt_A",
                      "DefQBRating_A",
                      "TODifference",
                      "PPG_H",
                      "DefPPG_H",
                      "PPG_A",
                      "DefPPG_A")]

knn_predAllData <- predAllData[,
                       c("Result",
                         "W_H",
                         "W_A",
                         "RushYds_H",
                         "RushTD_H",
                         "RushYA_H",
                         "RushYG_H",
                         "PassInt_H",
                         "DefRushYG_H",
                         "DefInt_H",
                         "DefQBRating_H",
                         "RushYds_A",
                         "RushTD_A",
                         "RushYA_A",
                         "RushYG_A",
                         "PassInt_A",
                         "DefRushTD_A",
                         "DefRushYG_A",
                         "DefInt_A",
                         "DefQBRating_A",
                         "TODifference",
                         "PPG_H",
                         "DefPPG_H",
                         "PPG_A",
                         "DefPPG_A")]
    
remove(allData)
remove(predAllData)

save(knn_predAllData,file = "~/Progs/R/NFL/knn_predictionData.RData")
save(knn_allData,file = "~/Progs/R/NFL/knn_allData.RData")


########################
#
#Point Differential
#
########################
install.packages("class")
install.packages("gmodels")
library(class)
library(gmodels)

load("~/Progs/R/NFL/knn_allData.RData")
load("~/Progs/R/NFL/knn_predictionData.RData")

knn_allData_zScore <- as.data.frame(scale(knn_allData[-1]))
knn_predAllData_zScore <- as.data.frame(scale(knn_predAllData[-1]))

knn_trainingResult <- knn_allData[,1]
knn_testResult <- knn_predAllData[,1]

#the first knn with the point differential
knn_prediction <- knn_predict(knn_allData_zScore, knn_predAllData_zScore, knn_trainingResult, 7)

summary(knn_prediction)

jpeg("~/Progs/R/NFL/KNN/knnPrediction.jpg")
plot(x=knn_prediction,xlab="Predicted Differential",ylab="Amount",main="K-NN Prediction (k = 7)")
dev.off()

jpeg("~/Progs/R/NFL/KNN/knnActual.jpg")
plot(as.factor(knn_testResult),xlab="Actual Differntial",ylab="Amount",main="Actual Outcomes")
dev.off()

#########################
#
#now getting if I picked the right team with the 
#stats used from linear regressions
#
#########################
testing_outcome <- 0
#getting vector of which team won each game with no score differential
for(i in 1:length(knn_trainingResult)){
    testing_outcome[i] <- knn_trainingResult[i]/abs(knn_trainingResult[i])
}

#the second knn with home/away only
knn_HA_prediction <- knn_predict(knn_allData_zScore, knn_predAllData_zScore, testing_outcome, 12) 

summary(knn_HA_prediction)

training_outcome <- 0
#getting vector of which team won each game with no score differential
for(i in 1:length(knn_testResult)){
    training_outcome[i] <- knn_testResult[i]/abs(knn_testResult[i])
}

sink("~/Progs/R/NFL/KNN/knnPrediction.txt")
CrossTable(x = knn_testResult,
           y = knn_HA_prediction,
           prop.chisq = FALSE,
           prop.r=FALSE,
           prop.c=FALSE,
           prop.t=FALSE)
sink()

sink("~/Progs/R/NFL/KNN/knnAccuracy.txt")
paste(paste("Accuracy: %",round((137+246)/532*100,2),sep=""),"with k=12")
CrossTable(x = training_outcome,
           y = knn_HA_prediction,
           prop.chisq = FALSE,
           prop.r=FALSE,
           prop.c=FALSE,
           prop.t=FALSE)
sink()

######################################
#
#This is for truly all of the data on
#each team going into the KNN run
#
######################################

load("~/Progs/R/NFL/allData.RData")
load("~/Progs/R/NFL/predictionData.RData")

knn_allData <- allData
knn_predAllData <- predAllData

remove (allData)
remove (predAllData)

#getting rid of text field and duplicate stats
for(i in 1:10){
    knn_allData <- knn_allData[,-1]
    knn_predAllData <- knn_predAllData[,-1]
}
knn_allData <- knn_allData[,-2]
knn_predAllData <- knn_predAllData[,-2]
knn_allData <- knn_allData[,-32]
knn_predAllData <- knn_predAllData[,-32]
knn_allData <- knn_allData[,-63]
knn_predAllData <- knn_predAllData[,-63]
for(i in 1:4){
    knn_allData <- knn_allData[,-69]
    knn_predAllData <- knn_predAllData[,-69]
}

knn_allData_zScore <- as.data.frame(scale(knn_allData[-1]))
knn_predAllData_zScore <- as.data.frame(scale(knn_predAllData[-1]))

knn_trainingResult <- knn_allData[,1]
knn_testResult <- knn_predAllData[,1]

knn_allStatsPrediction <- knn_predict(knn_allData_zScore, knn_predAllData_zScore, knn_trainingResult, 7)

summary(knn_allStatsPrediction)

jpeg("~/Progs/R/NFL/KNN/knnAllStatsPrediction.jpg")
plot(x=knn_allStatsPrediction,xlab="Predicted Differential",ylab="Amount",main="K-NN All Stats Prediction (k = 7)")
dev.off()

##############################
#
#now getting if I oicked the right team
#with all of the statistics
#
##############################
testing_outcome <- 0
#getting vector of which team won each game with no score differential
for(i in 1:length(knn_trainingResult)){
    testing_outcome[i] <- knn_trainingResult[i]/abs(knn_trainingResult[i])
}
training_outcome <- 0
#getting vector of which team won each game with no score differential
for(i in 1:length(knn_testResult)){
    training_outcome[i] <- knn_testResult[i]/abs(knn_testResult[i])
}


knn_HA_AllStatsPrediction <- knn_predict(knn_allData_zScore, knn_predAllData_zScore, testing_outcome, 7) 


sink("~/Progs/R/NFL/KNN/knnAllStatsPrediction.txt")
CrossTable(x = knn_testResult,
           y = knn_HA_AllStatsPrediction,
           prop.chisq = FALSE,
           prop.r=FALSE,
           prop.c=FALSE,
           prop.t=FALSE)
sink()

sink("~/Progs/R/NFL/KNN/knnAllStatsAccuracy.txt")
paste(paste("Accuracy: %",round((130+250)/532*100,2),sep=""),"with k=7")
CrossTable(x = training_outcome,
           y = knn_HA_AllStatsPrediction,
           prop.chisq = FALSE,
           prop.r=FALSE,
           prop.c=FALSE,
           prop.t=FALSE)
sink()

###########################################################


knn_predict <- function(all_zScore, pred_zScore, result, myK){
    return (knn(train = all_zScore, 
                          test = pred_zScore,
                          cl= result,
                          k=myK))
}
