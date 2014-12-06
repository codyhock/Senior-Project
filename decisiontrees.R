#Cody Hock
#This is the Decision Tree R script.

#getting the proper packages for the decision tree algorithm
install.packages("C50")
library(C50)
library(gmodels)

#loading in previous data from KNN
load("~/Progs/R/NFL/knn_allData.RData")
load("~/Progs/R/NFL/knn_predictionData.RData")

#setting new data frames to be equal to these from KNN
#because I will need to delete from these data frames
treesData <- knn_allData
treesPredData <- knn_predAllData

#clearing the KNN data frames from teh environment
remove(knn_allData)
remove(knn_predAllData)

#Only need to do once
#this was removing non-integer fields
if(FALSE){
    treesData<-treesData[-2]
    treesPredData<-treesPredData[-2]
    treesData<-treesData[-2]
    treesPredData<-treesPredData[-2]
    
    treesData<-treesData[-4]
    treesPredData<-treesPredData[-4]
    treesData<-treesData[-11]
    treesPredData<-treesPredData[-11]
    treesData<-treesData[-15]
    treesPredData<-treesPredData[-15]
    treesData<-treesData[-18]
    treesPredData<-treesPredData[-18]
}
#storing the actual results
test <- treesPredData
test$Result <- test$Result/abs(test$Result)

#getting factor (type of vector) of results at either 1 or -1
resultFactor <- as.factor(treesData$Result/abs(treesData$Result))

#the actual decision tree algorithm returning the model
#this one is only doing which team wins, the next is doing point differnetial
DecisionModel <- C5.0(treesData[-1],resultFactor,trials=12)

#printing info about the model
DecisionModel
summary(DecisionModel)

#running the prediciton of the model on the test data set
DecisionPrediction <- predict(DecisionModel,test)
#############
#
#for the frequency of each
#
#############
#getting factor of the results that actually occured
largeResults <- as.factor(treesData$Result)

#this model is using the actual point differential, not just which team
DecisionModel_frequency <- C5.0(treesData[-1],largeResults,trials=7)

#printing info about the model
DecisionModel_frequency
summary(DecisionModel_frequency)

#running orediction
DecisionPrediction_frequency <- predict(DecisionModel_frequency,treesPredData)

#saving image of prediciton
jpeg("~/Progs/R/NFL/decision/trees.jpg")
plot(x=DecisionPrediction_frequency,xlab="Predicted Differential",ylab="Amount",main="Decision Trees Frequency (trials = 7)")
dev.off()

###############
#
#saving files
#
###############
sink("~/Progs/R/NFL/decision/treesAccuracy.docx")
paste("Accuracy: %",round(100*((123+252)/532),2))
CrossTable(test$Result, DecisionPrediction, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,prop.t = FALSE,dnn = c('Actual Result','Predicted Result'))
sink()
sink("~/Progs/R/NFL/decision/treesModel.docx")
DecisionModel
summary(DecisionModel)
sink()