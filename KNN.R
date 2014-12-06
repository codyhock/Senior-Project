#Cody Hock
#This is the R script for K-Nearest Neighbors.
#Unlike regression, you cannot specify what columns, instead
#every column is taken into the algorithm. Therefore there is
#further work to be done on the data frame I used here.

#########################
#
#Getting data for the KNN
#
#########################
load("~/Progs/R/NFL/allData.RData")
load("~/Progs/R/NFL/predictionData.RData")

#the reason I am only getting certain data here is because these are the 
#stats I found to be most significant in my linear regressions. I will
#try combinations of these stats.
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
#clearing lager data frames from the environment    
remove(allData)
remove(predAllData)

#saving the smaller data frames to be loaded in other R scripts
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

#this is getting the data to train without the observed result
#then scaling each column by its Z score so no one column has a larger
#impact on the model
knn_allData_zScore <- as.data.frame(scale(knn_allData[-1]))
knn_predAllData_zScore <- as.data.frame(scale(knn_predAllData[-1]))

#this stores the results that were ignored above
knn_trainingResult <- knn_allData[,1]
knn_testResult <- knn_predAllData[,1]

#the first knn with the point differential
knn_prediction <- knn_predict(knn_allData_zScore, knn_predAllData_zScore, knn_trainingResult, 7)

#printing info of prediction
summary(knn_prediction)

#saving the images
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

#getting vector of which team won each game with no score differential
testing_outcome <- 0
testing_outcome <- getOutcome(testing_outcome, knn_trainingResult)

#the second knn with home/away only
knn_HA_prediction <- knn_predict(knn_allData_zScore, knn_predAllData_zScore, testing_outcome, 12) 

summary(knn_HA_prediction)

#getting vector of which team won each game with no score differential
training_outcome <- 0
training_outcome <- getOutcome(training_outcome, knn_testResult)

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
if(FALSE){
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
}

#Data frames minus the results
knn_allData_zScore <- as.data.frame(scale(knn_allData[-1]))
knn_predAllData_zScore <- as.data.frame(scale(knn_predAllData[-1]))

#storing results ignored from above
knn_trainingResult <- knn_allData[,1]
knn_testResult <- knn_predAllData[,1]

#predicting
knn_allStatsPrediction <- knn_predict(knn_allData_zScore, knn_predAllData_zScore, knn_trainingResult, 7)

summary(knn_allStatsPrediction)

jpeg("~/Progs/R/NFL/KNN/knnAllStatsPrediction.jpg")
plot(x=knn_allStatsPrediction,xlab="Predicted Differential",ylab="Amount",main="K-NN All Stats Prediction (k = 7)")
dev.off()

##############################
#
#now getting if I picked the right team
#with all of the statistics
#
##############################

#getting vector of which team won each game with no score differential
testing_outcome <- 0
testing_outcome <- getOutcome(testing_outcome,knn_trainingResult)

#getting vector of which team won each game with no score differential
training_outcome <- 0
training_outcome <- getOutcome(training_outcome,knn_testResult)

#getting the actual predition from KNN
knn_HA_AllStatsPrediction <- knn_predict(knn_allData_zScore, knn_predAllData_zScore, testing_outcome, 7) 

#saving to the appropriate files with sink()
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

#################################
#
#functions
#
#################################

#my function that calls the actuall knn model that R provides
#saving writing code from above
knn_predict <- function(all_zScore, pred_zScore, result, myK){
    return (knn(train = all_zScore, 
                          test = pred_zScore,
                          cl= result,
                          k=myK))
}
#getting vector of which team won each game with no score differential
getOutcome <- function(outcomeVector, resultVector){
    for(i in 1:length(resultVector)){
        outcomeVector[i] <- resultVector[i]/abs(resultVector[i])
    }
    return(outcomeVector)
}
