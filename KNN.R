#########################
#
#Skip Ahead, this has been executed
#and you only need to do it once
#
#########################
if(FALSE){
    load("~/Progs/R/NFL/allData.RData")
    load("~/Progs/R/NFL/predictionData.RData")
    
    knn_allData <- allData
    knn_predAllData <- predAllData
    
    remove(allData)
    remove(predAllData)
    
    for(i in 1:10){
        knn_allData <- knn_allData[,-1]
        knn_predAllData <- knn_predAllData[,-1]
    }for(i in 1:6){
        knn_allData <- knn_allData[,-2]
        knn_predAllData <- knn_predAllData[,-2]
    }for(i in 1:3){
        knn_allData <- knn_allData[,-6]
        knn_predAllData <- knn_predAllData[,-6]
    }for(i in 1:7){
        knn_allData <- knn_allData[,-7]
        knn_predAllData <- knn_predAllData[,-7]
        
    }for(i in 1:4){
        knn_allData <- knn_allData[,-8]
        knn_predAllData <- knn_predAllData[,-8]
    }
    knn_allData <- knn_allData[,-9]
    knn_predAllData <- knn_predAllData[,-9]
    for(i in 1:8){
        knn_allData <- knn_allData[,-10]
        knn_predAllData <- knn_predAllData[,-10]
    }for(i in 1:3){
        knn_allData <- knn_allData[,-14]
        knn_predAllData <- knn_predAllData[,-14]
    }for(i in 1:5){
        knn_allData <- knn_allData[,-15]
        knn_predAllData <- knn_predAllData[,-15]
    }
    knn_allData <- knn_allData[,-16]
    knn_predAllData <- knn_predAllData[,-16]
    for(i in 1:6){
        knn_allData <- knn_allData[,-17]
        knn_predAllData <- knn_predAllData[,-17]
    }for(i in 1:2){
        knn_allData <- knn_allData[,-18]
        knn_predAllData <- knn_predAllData[,-18]
    }for(i in 1:3){
        knn_allData <- knn_allData[,-19]
        knn_predAllData <- knn_predAllData[,-19]
    }for(i in 1:4){
        knn_allData <- knn_allData[,-23]
        knn_predAllData <- knn_predAllData[,-23]
    }
    
    save(knn_predAllData,file = "~/Progs/R/NFL/knn_predictionData.RData")
    save(knn_allData,file = "~/Progs/R/NFL/knn_allData.RData")
}

########################
#
#Start Here
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
knn_prediction <- knn(train = knn_allData_zScore, 
                      test = knn_predAllData_zScore,
                      cl= knn_trainingResult,
                      k=7)

summary(knn_prediction)

jpeg("~/Progs/R/NFL/KNN/knnPrediction.jpg")
plot(x=knn_prediction,xlab="Predicted Differential",ylab="Amount",main="K-NN Prediction (k = 7)")
dev.off()

jpeg("~/Progs/R/NFL/KNN/knnActual.jpg")
plot(as.factor(knn_testResult),xlab="Actual Differntial",ylab="Amount",main="Actual Outcomes")
dev.off()

#########################
#
#now getting if I picked the right team
#
#########################
testing_outcome <- 0
for(i in 1:length(knn_trainingResult)){
    testing_outcome[i] <- knn_trainingResult[i]/abs(knn_trainingResult[i])
}

#the second knn with home/away only
knn_HA_prediction <-knn(train = knn_allData_zScore, 
                         test = knn_predAllData_zScore,
                         cl= testing_outcome,
                         k=12) 

summary(knn_HA_prediction)

training_outcome <- 0
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
paste("Accuracy: %",round((125+244)/532*100,2),sep="")
CrossTable(x = training_outcome,
           y = knn_HA_prediction,
           prop.chisq = FALSE,
           prop.r=FALSE,
           prop.c=FALSE,
           prop.t=FALSE)

sink()

