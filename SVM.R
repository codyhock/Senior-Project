#Cody Hock
#The Support Vector Machines R script

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
#I ran tests below for adding custom 
#constraints as well as types. It turns out the best for
#this type of dataset is to use the
#defaults, which are: epsilon-regresiion with a cost
#of 1.
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
#printing results of each
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

######################
#
#Testing the values for C
#higher the C-value (cost of violating constraints),
#the higher the penalty. So higher C results in narrower 
#margins.
#
########################

TEST_polydot <- ksvm(Result ~
                         .,
                     data = svm_allData,
                     C=3,
                     type="C-svc",
                     kernal = "polydot")
TEST_poly_predictiopn <- predict(TEST_polydot,svm_predAllData)
TEST_poly_agreement <- TEST_poly_predictiopn/abs(TEST_poly_predictiopn) == svm_predAllData$Result/abs(svm_predAllData$Result)
table(TEST_poly_agreement)
prop.table(table(TEST_poly_agreement))

#

TEST_vanilla <- ksvm(Result ~
                         .,
                     data = svm_allData,
                     C=15,
                     kernal = "vanilladot")
TEST_vanilla_prediction <- predict(TEST_vanilla,svm_predAllData)
TEST_vanilla_agreement <- TEST_vanilla_prediction/abs(TEST_vanilla_prediction) == svm_predAllData$Result/abs(svm_predAllData$Result)
table(TEST_vanilla_agreement)
prop.table(table(TEST_vanilla_agreement))

#

TEST_rfb<- ksvm(Result ~
                         .,
                     data = svm_allData,
                     C=5,
                     kernal = "rfbdot")
TEST_rfb_prediction <- predict(TEST_rfb,svm_predAllData)
TEST_rfb_agreement <- TEST_rfb_prediction/abs(TEST_rfb_prediction) == svm_predAllData$Result/abs(svm_predAllData$Result)
table(TEST_rfb_agreement)
prop.table(table(TEST_rfb_agreement))

#

TEST_tanh<- ksvm(Result ~
                    .,
                data = svm_allData,
                C=15,
                kernal = "tanhdot")
TEST_tanh_prediction <- predict(TEST_tanh,svm_predAllData)
TEST_tanh_agreement <- TEST_tanh_prediction/abs(TEST_tanh_prediction) == svm_predAllData$Result/abs(svm_predAllData$Result)
table(TEST_tanh_agreement)
prop.table(table(TEST_tanh_agreement))
