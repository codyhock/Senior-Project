#using the same data from the linear regessions and Nearest Neighbors
load("~/Progs/R/NFL/knn_allData.RData")
load("~/Progs/R/NFL/knn_predictionData.RData")

svm_allData <- knn_allData
svm_predAllData <- knn_predAllData

remove(knn_allData)
remove(knn_predAllData)

install.packages("kernlab")
library(kernlab)

###################
#
#the models for the svm algorithm
#
###################

#radial basis
svm_rbfdot <- ksvm(Result ~
                      .,
                  data = svm_allData,
                  kernal = "rbfdot")
#polynomial
svm_polydot <- ksvm(Result ~
                       .,
                   data = svm_allData,
                   kernal = "polydot")
?ksvm
#hyperbolic tangentsigmoid
svm_tanhdot <- ksvm(Result ~
                       .,
                   data = svm_allData,
                   kernal = "tanhdot")
#linear
svm_vanilladot <- ksvm(Result ~
                       .,
                   data = svm_allData,
                   kernal = "vanilladot")

svm_rbfdot
svm_polydot
svm_tanhdot
svm_vanilladot

###################
#
#now the predictions
#these return a result for each row of the data frame
#
###################

svm_rbfdot_prediction <- predict(svm_rbfdot,svm_predAllData)

svm_polydot_prediction <- predict(svm_polydot,svm_predAllData)

svm_tanhdot_prediction <- predict(svm_tanhdot,svm_predAllData)

svm_vanilladot_prediction <- predict(svm_vanilladot,svm_predAllData)

#######################
#
#showing how well the predictions went
#
#######################
rfbdot_agreement <- svm_rbfdot_prediction/abs(svm_rbfdot_prediction) == svm_predAllData$Result/abs(svm_predAllData$Result)
table(rfbdot_agreement)
prop.table(table(rfbdot_agreement))

polydot_agreement <- svm_polydot_prediction/abs(svm_polydot_prediction) == svm_predAllData$Result/abs(svm_predAllData$Result)
table(polydot_agreement)
prop.table(table(polydot_agreement))

tanhdot_agreement <- svm_tanhdot_prediction/abs(svm_tanhdot_prediction) == svm_predAllData$Result/abs(svm_predAllData$Result)
table(tanhdot_agreement)
prop.table(table(tanhdot_agreement))

vanilladot_agreement <- svm_vanilladot_prediction/abs(svm_vanilladot_prediction) == svm_predAllData$Result/abs(svm_predAllData$Result)
table(vanilladot_agreement)
prop.table(table(vanilladot_agreement))


###################
#
#saving data for display
#
###################
sink("~/Progs/R/NFL/SVM/SVMaccuracy.docx")
prop.table(table(rfbdot_agreement))
prop.table(table(polydot_agreement))
prop.table(table(tanhdot_agreement))
prop.table(table(vanilladot_agreement))
sink()

jpeg("~/Progs/R/NFL/SVM/vanilladot.jpg")
plot(svm_vanilladot_prediction,svm_predAllData$Result,main="Score Differential",ylab="Actual",xlab="Predicted",xlim=c(-60,60),ylim=c(-60,60))
abline(0,1,col="red")
abline(v=0,col="blue")
abline(h=0,col="blue")
dev.off()