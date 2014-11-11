install.packages("C50")
library(C50)
library(gmodels)

load("~/Progs/R/NFL/knn_allData.RData")
load("~/Progs/R/NFL/knn_predictionData.RData")

treesData <- knn_allData
treesPredData <- knn_predAllData

remove(knn_allData)
remove(knn_predAllData)

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
test <- treesPredData
test$Result <- test$Result/abs(test$Result)

resultFactor <- as.factor(treesData$Result/abs(treesData$Result))

DecisionModel <- C5.0(treesData[-1],resultFactor,trials=12)

DecisionModel
summary(DecisionModel)

DecisionPrediction <- predict(DecisionModel,test)
#############
#
#for the frequency of each
#
#############
largeResults <- as.factor(treesData$Result)

DecisionModel_frequency <- C5.0(treesData[-1],largeResults,trials=7)
DecisionModel_frequency
summary(DecisionModel_frequency)

DecisionPrediction_frequency <- predict(DecisionModel_frequency,treesPredData)


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