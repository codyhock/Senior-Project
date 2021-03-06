#Cody Hock
#This is the nueral network that I never fully finished

install.packages('neuralnet')
library("neuralnet")

#using the same data from the linear regessions and Nearest Neighbors
load("~/Progs/R/NFL/knn_allData.RData")
load("~/Progs/R/NFL/knn_predictionData.RData")

#normalizing
neuralTrain <- as.data.frame(lapply(knn_allData,normalize)) 
neuralTest <- as.data.frame(lapply(knn_predAllData,normalize))
knn_allData$Result

withoutNorm<-neuralTrain
withoutNorm$Result<-knn_allData$Result
#neuralTrain <- neuralTrain[-17]
#neuralTest <- neuralTest[-17]
save(neuralTrain,file = "~/Progs/R/NFL/neuralTrain.RData")
save(neuralTest,file = "~/Progs/R/NFL/neuralTest.RData")

summary(knn_allData$Result)
summary(nueralTrain$Result)
####################################3
#testing here
#Trying to understand neuralnet
#
#####################################

if(FALSE){
    NeuralTraining <- knn_allData
    NeuralTesting <- knn_predAllData
    for (i in 1:3){
        NeuralTraining<-NeuralTraining[-2]
        NeuralTesting<-NeuralTesting[-2]
    }
    NeuralTraining<-NeuralTraining[-9]
    NeuralTesting<-NeuralTesting[-9]
    NeuralTraining<-NeuralTraining[-17]
    NeuralTesting<-NeuralTesting[-17]
    NeuralTraining<-NeuralTraining[-18]
    NeuralTesting<-NeuralTesting[-18]
    NeuralTraining<-NeuralTraining[-19]
    NeuralTesting<-NeuralTesting[-19]
}
TestModel <- neural(1)
TestModel
plot(TestModel)
TestResults <- compute(TestModel,NeuralTesting[-1])
res <- data.frame(actual=NeuralTesting$Result,predicted=TestResults$net.result)
res
##################
#
#1 hidden neuron
#all subsequent calls follow this pattern
#
##################

#creating the model
Neural_Model <- neural(1)
Neural_Model
#saving the image of the network
jpeg("~/Progs/R/NFL/neural/neuralLevel1.jpg")
plot(Neural_Model)
dev.off()

newTest <- neuralTest
newTest <- newTest[-19]

#now determining how well this predicted the outcomes
model_results <- compute(Neural_Model,newTest[-1])
predicted_result <- model_results$net.result

#combining matricies of actual results and predicted results
cor(predicted_result, neuralTest$Result)

####################
#
#2 hidden nodes
#
####################

Neural_Model_2 <- neural(2)
Neural_Model_2
jpeg("~/Progs/R/NFL/neural/neuralLevel2.jpg")
plot.nn(Neural_Model_2)
dev.off()

second_model_results <- compute(Neural_Model_2, newTest[-1])
second_predicted_result <- second_model_results$net.result

cor(second_predicted_result, neuralTest$Result)


####################
#
#now with 3 hidden nodes
#
####################

Neural_Model_3 <- neural(3)
Neural_Model_3
jpeg("~/Progs/R/NFL/neural/neuralLevel3.jpg")
plot.nn(Neural_Model_3)
dev.off()

third_model_results <- compute(Neural_Model_3,newTest[-1])
third_predicted_result <- third_model_results$net.result

cor(third_predicted_result, neuralTest$Result)

#######################
#
#4
#
#######################

Neural_Model_4 <- neural(4)
Neural_Model_4
jpeg("~/Progs/R/NFL/neural/neuralLevel4.jpg")
plot.nn(Neural_Model_4)
dev.off()

fourth_model_results <- compute(Neural_Model_4,newTest[-1])
fourth_predicted_result <- fourth_model_results$net.result

cor(fourth_predicted_result, neuralTest$Result)

#######################
#
#5
#
#######################

Neural_Model_5 <- neural(5)
Neural_Model_5
jpeg("~/Progs/R/NFL/neural/neuralLevel5.jpg")
plot.nn(Neural_Model_5)
dev.off()

fifth_model_results <- compute(Neural_Model_5,newTest[-1])
fifth_predicted_result <- fifth_model_results$net.result

cor(fifth_predicted_result, neuralTest$Result)
################
#
#dumping to file
#
################
t <- rbind(cor(predicted_result, neuralTest$Result),cor(second_predicted_result, neuralTest$Result),cor(third_predicted_result, neuralTest$Result),cor(fourth_predicted_result, neuralTest$Result),cor(fifth_predicted_result, neuralTest$Result))

sink("~/Progs/R/NFL/neural/NeuralCorrelations.docx")
paste("With 1 layer of abstraction: ", t[1,1])
Neural_Model[13]
paste("With 2 layers of abstraction: ", t[2,1])
Neural_Model_2[13]
paste("With 3 layers of abstraction: ", t[3,1])
Neural_Model_3[13]
paste("With 4 layers of abstraction: ", t[4,1])
Neural_Model_4[13]
paste("With 5 layers of abstraction: ", t[5,1])
Neural_Model_5[13]
sink()

#####################
#
#functions
#
####################

#normalizing the data frames
normalize <- function(column){
    return ((column-min(column))/(max(column)-min(column)))
}

#the call to the neuralnet function in R
#returns the model. H is number of hidden neurons in each layer
neural <- function(h){
    return (neuralnet(Result ~
                          RushTD_H + 
                          RushYA_H + 
                          RushYG_H + 
                          PassInt_H + 
                          DefRushYG_H +
                          DefInt_H + 
                          DefQBRating_H + 
                          RushTD_A + 
                          RushYA_A + 
                          RushYG_A + 
                          PassInt_A + 
                          DefRushTD_A + 
                          DefRushYG_A +
                          DefInt_A + 
                          DefQBRating_A + 
                          PPG_H + 
                          PPG_A
                      ,
                      data = neuralTrain,
                      hidden = h))
}
