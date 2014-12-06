#Cody Hock
#This R script is not a part of my final predictions,
#it is rather just one of the many scripts I wrote to teach myself
#how R works. I thought I would include one of them in its
#raw form to show I did write more code in order to learn the language.
#This is a simple Linear regression for the home and away teams.

library(RMySQL)

con <- dbConnect(MySQL(), user='cody',password='',host='localhost',dbname='NFL')

scores <- "select * from scores where year < 2012"
testScores <- "select * from scores where year > 2011"

testDF <- dbGetQuery(con,testScores)
myDF <- dbGetQuery(con,scores)

dbDisconnect(con)

#going through the data frame and replacing team names with -1(away) and 1(home)
#remember, team1 won the game, team2 lost
for(i in 1:nrow(myDF)){
    if(myDF[i,4] == '@'){
        myDF[i,3]=-1
        myDF[i,5]=1
    }
    else{
        myDF[i,3]=1
        myDF[i,5]=-1        
    }
}
for(i in 1:nrow(testDF)){
    if(testDF[i,4] == '@'){
        testDF[i,3]=-1
        testDF[i,5]=1
    }
    else{
        testDF[i,3]=1
        testDF[i,5]=-1        
    }
}

#without the 4th column which contains characters '@'
myMatrix <- as.matrix(myDF[,-4])
testMatrix <- as.matrix(testDF[,-4])
class(testMatrix)<-"numeric"
class(myMatrix)<-"numeric"
nrow(myMatrix)

myMatrix[267,]
myMatrix <- cbind(myMatrix,myMatrix[,"team1"])
testMatrix <- cbind(testMatrix,testMatrix[,"team1"])

testData <- myMatrix[,7:11]
predData <- testMatrix[,7:11]

#reg(mat = testData[,-5], y = testData[,5])

########
#now building a model
########
testModel <- as.data.frame(testData)
colnames(testModel)[1] <- "HomeYards"
colnames(testModel)[2] <- "HomeTO"
colnames(testModel)[3] <- "AwayYards"
colnames(testModel)[4] <- "AwayTO"
colnames(testModel)[5] <- "Result"

predModel <- as.data.frame(predData)
colnames(predModel)[1] <- "HomeYards"
colnames(predModel)[2] <- "HomeTO"
colnames(predModel)[3] <- "AwayYards"
colnames(predModel)[4] <- "AwayTO"
colnames(predModel)[5] <- "Result"


#I need to set up the data frame to be in columns for home and away, not win and loss
for(i in 1:nrow(testModel)){
    #if away team won, flip the data
    if(testModel[i,5] == -1){
        yards <- testModel[i,1]
        to <- testModel[i,2]
        testModel[i,1] = testModel[i,3]
        testModel[i,2] = testModel[i,4]
        testModel[i,3] <- yards
        testModel[i,4] <- to
        remove(yards)
        remove(to)
    }
}
#again for my prediciton model
for(i in 1:nrow(predModel)){
    #if away team won, flip the data
    if(predModel[i,5] == -1){
        yards <- predModel[i,1]
        to <- predModel[i,2]
        predModel[i,1] = predModel[i,3]
        predModel[i,2] = predModel[i,4]
        predModel[i,3] <- yards
        predModel[i,4] <- to
        remove(yards)
        remove(to)
    }
}

testModel$HomeTO2 <- testModel$HomeTO^2
testModel$AwayTO2 <- testModel$AwayTO^2
testModel <- testModel[,-8]

#Page 179
model <- 
    lm(Result ~ 
           HomeYards + 
           HomeYards*AwayYards +
           HomeTO +
           HomeTO*AwayYards +
           HomeTO*HomeYards + 
           HomeTO2 + 
           HomeTO2*AwayYards +
           HomeTO2*HomeYards + 
           AwayYards + 
           AwayTO +
           AwayTO*AwayYards +
           AwayTO*HomeYards +
           AwayTO2 +
           AwayTO2*AwayYards + 
           AwayTO2*HomeYards, 
       data = testModel)

model
summary(model)
sink("~/Progs/R/NFL/HomeAwayModel.txt")
summary(model)
sink()

yardModel <- lm(Result ~ HomeYards + AwayYards , data = testModel)
yardModel
summary(yardModel)

toModel <- lm(Result ~ HomeTO + AwayTO , data = testModel)
toModel
summary(toModel)


plot(model)


predModel$HomeTO2 <- predModel$HomeTO^2
predModel$AwayTO2 <- predModel$AwayTO^2
#predict outcomes
pred  <- predict(model, predModel)
predModel
summary(pred)
plot(pred,predModel$Result,main="Home (1) VS Away (-1)",ylab="Actual",xlab="Predicted")
correct <-table(predModel$Result,pred)
barplot(correct)
length(pred)

hist(pred)

save(pred,file="HomeAwayPrediction.RData")

#
#
pred


#number of games played is 3186
#Home team wins = 1826
#Away team wins = 1360

#colSums(myMatrix)
#myMatrix[1,2]
#homeWins <- 0
#nrow(myMatrix)
#for (i in 1:nrow(myMatrix)){
#    if(myMatrix[i,3] == 1){
#        homeWins <- homeWins + 1
#    }
#}
#homeWins



#regression function
#page 168-171
#y = b0 + b1x1 + b2x2 +...+ bixi + eps
#Y = XB + eps
#B = (X^t X)^-1 X^t Y
reg <- function(mat,y){
    #creates new column and fills with 1
    mat <- cbind (Intercept = 1, mat)
    # solve() is the inverse of matrix
    # t() is transpose
    # %*% is matrix multiplication 
    solve (t(mat) %*% mat) %*% t(mat) %*% y
}

