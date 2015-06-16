library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(nnet)
library(MASS)
library(e1071)
library(corrplot)
library(randomForest)
library(gbm)
require(caret)

source('cleanData.R')
source('plotData.R')

#---------------------------------------------------------------------
#read in the data
train <- read.csv('train.csv')
test  <-read.csv('test.csv')

#-------------------TRY TO GET PASSENGER ETHNICITY BY USING NAME AS A PROXY------------------------
#I get the 2010 US census data in an effort to reconstitute the ethnicity of the individual passengers
names  <-read.csv('USCensusNames2000.csv',stringsAsFactors = FALSE)
names[6:11]<-as.numeric(unlist(names[6:11]))


#--------------------------------------------------------------------------
#
#First clean the data somewhat and create a bunch of features.
ageBreaks <- c(0,2,5,10,15,18,30,40,50,60,70,80,90)
priceBreaks <- c(seq(from=0,by=5, to=95),seq(from=100,by=10, to=190),seq(from=200,by=20, to=540))
#clean the data and create new features for the full set. Use the names database from US census to get a guess at ethnicity.
#New features we create are as follows:
#GroupFeat-> Number of passengers (including the passengers in question) travelling together
#EthnicFeat-> Probable ethnicity of passenger (derived using the US census data of surnames)
#FarePassenger-> The fare prices are per ticket, but many tickets are for multiple passengers. This has the ticket price normalised by the number of passengers travelling on the ticket
#MFCoupleFeat ->Is the person travelling as a couple? Excludes people travelling with parents or children
#FamilyFeat ->Is the person travelling in a family?
#TitleFeat -> Looking at the data suggests that certain social classes are more likely to survive. This creates a factor based on persons title
#CabinFeat -> If information about cabin the person was travelling in is available, then this is captured here.
#PriceFeat -> Different price brackets
#AgeFeat -> Different Age brackets


fullData<-(cleanData(train,test,names,ageBreaks,priceBreaks))
#clean the data and create new features for the full set.
fullData$TitleFeat<-as.factor(fullData$TitleFeat)
fullData$GroupFeat<-as.factor(fullData$GroupFeat)
fullData$MFCoupleFeat<-as.factor(fullData$MFCoupleFeat)
fullData$FamilyFeat<-as.factor(fullData$FamilyFeat)
fullData$CabinFeat<-as.factor(fullData$CabinFeat)
fullData$EthnicFeat<-as.factor(fullData$EthnicFeat)


#--------------------------------------------------------------------------

trainFeat <- (fullData[1:891,])
testFeat <- (fullData[892:1309,])

plotData(trainFeat)

#-------Create a formula that we will use for the fitting algorithms.
formula <-Survived ~  Pclass + Sex + FarePassenger+ Age + Embarked + EthnicFeat + TitleFeat + GroupFeat + MFCoupleFeat + FamilyFeat + CabinFeat




#--------------------Recursive Partitioning Solution------------------------------------------------------------------
#lets explore this model a little with recursive partioning.
formula <-Survived ~  Pclass + Sex + FarePassenger+ Age + Embarked + EthnicFeat + TitleFeat + GroupFeat + MFCoupleFeat + FamilyFeat + CabinFeat
tunedRpart<-tune.rpart(formula, data=as.data.frame(trainFeat), minsplit = c(25,50,75,100),
           minbucket = c(10,20,30,40,50), cp = c(0,0.1,0.2,0.3,0.4,0.5), maxcompete = c(0,1), maxsurrogate = c(0,1),
           usesurrogate = c(0,1), xval = c(0,1,2,3,4))
fit <- rpart(formula, data=as.data.frame(trainFeat), method="class",control=rpart.control(minsplit=100,cp=0,minbucket=10,maxcompete=0,maxsurrogate=0,usesurrogate=0,xval=0))
Prediction <- predict(fit, as.data.frame(trainFeat), type = "class")
results.matrix <- confusionMatrix(Prediction, trainFeat$Survived)
accuracyRpart<-results.matrix$overall[1]
pdf("rTree1.pdf")
fancyRpartPlot(fit)
dev.off()

PredictionRpart <- predict(fit, as.data.frame(testFeat), type = "class")
submit <- data.frame(PassengerID = testFeat$PassengerId, Survived = (PredictionRpart))
write.csv(submit, file = "submitCJC.csv", row.names = FALSE)

#842   ↓110 	
#Charles Copley
#0.79426 	21 	Tue, 16 Jun 2015 04:50:39 (-9.7d)
#Your Best Entry ↑
#Your submission scored 0.79426, which is not an improvement of your best score. Keep trying! 
#-----------------------------------------------------------------------------------------------------

#--------------------K nearest Neighbours-------------------------------------------------------------
formula <-Survived ~  Pclass + Sex + FarePassenger+ Age + Embarked + EthnicFeat + TitleFeat + GroupFeat + MFCoupleFeat + FamilyFeat + CabinFeat


#Do a quick rms error study of the effect of adding additional possible clusters i.e. increasing k in the k-means algorithm
kValsErr<-data.frame(error=numeric(30));
for(i in (1:30)){
  knnFit<-knn3(formula,trainFeat,k=i,, prob = FALSE)
  fitKnn<-round(as.data.frame(predict(knnFit,trainFeat, prob = FALSE)))
  kValsErr$error[i]<-sqrt(sum((aa<-round(fitKnn$'1') - trainFeat$Survived)^2))
}




#-------------------Support Vector Machine Solution---------------------------------------------------
formula <-Survived ~  Pclass + Sex + FarePassenger+ Age + Embarked + EthnicFeat + TitleFeat + GroupFeat + MFCoupleFeat + FamilyFeat + CabinFeat
#find the best SVM gamma and cost parameters
tuned <- tune.svm(formula, data=trainFeat, gamma = 10^(-3:3), cost = 10^(-2:4))
fitSVM  <- svm(formula, data = as.data.frame(trainFeat), 
               type="C-classification",
               kernel="radial",
               probability=T,
               gamma=0.1,
               cost=1) 
testFeat$Survived<-NULL

PredictionSVM <- predict(fitSVM, data=as.data.frame(trainFeat), type="C-classification")
results.matrix <- confusionMatrix((PredictionSVM), trainFeat$Survived)
accuracySVM<-results.matrix$overall[1]

PredictionSVM2 <- predict(fitSVM, newdata=as.data.frame(testFeat), type="C-classification")
submit <- data.frame(PassengerID = testFeat$PassengerId, Survived = (PredictionSVM2))
write.csv(submit, file = "submitCJC.csv", row.names = FALSE)

#836   ↓108 	
#Charles Copley
#0.79426 	15 	Mon, 15 Jun 2015 18:18:22 (-9.3d)
#Your Best Entry ↑
#Your submission scored 0.79426, which is not an improvement of your best score. Keep trying! 
#-----------------------------------------------------------------------------------------------------------


#------------------------Neural Network Solution------------------------------------------------------------
formula <-Survived ~  Pclass + Sex + FarePassenger+ Age + Embarked + EthnicFeat + TitleFeat + GroupFeat + MFCoupleFeat + FamilyFeat + CabinFeat
#find the best neural network
tuned <- tune.nnet(formula, data=trainFeat, size=c(5,10,15,30),decay=c(0,0.005,0.010),MaxNWts= 20000)
fitNnet <- svm(formula, data = as.data.frame(trainFeat), 
               type="C-classification",
               size=5,
               decay=0.005) 
PredictionNnet <- predict(fitNnet, data=as.data.frame(trainFeat), type="C-classification")
results.matrix <- confusionMatrix((PredictionNnet), trainFeat$Survived)
accuracyNnet<-results.matrix$overall[1]

PredictionNnet <- predict(fitNnet, newdata=as.data.frame(testFeat), type="C-classification")
submit <- data.frame(PassengerID = testFeat$PassengerId, Survived = (PredictionNnet))
write.csv(submit, file = "submitCJC.csv", row.names = FALSE)

#836   ↓108 	
#Charles Copley
#0.79426 	18 	Mon, 15 Jun 2015 18:42:49 (-9.3d)
#Your Best Entry ↑
#Your submission scored 0.78947, which is not an improvement of your best score. Keep trying! 
#-----------------------------------------------------------------------------------------------------------



#-------------------------Random Forest Solution---------------------------------------------------------------
formula <-Survived ~  Pclass + Sex + FarePassenger+ Age + Embarked + EthnicFeat + TitleFeat + GroupFeat + MFCoupleFeat + FamilyFeat + CabinFeat
trainFeatForest<-trainFeat
testFeatForest<-testFeat
trainFeatForest$Survived<-as.factor(trainFeatForest$Survived)
testFeatForest$Survived<-as.factor(testFeatForest$Survived)


tuned <- tune.randomForest(formula, data=trainFeat, ntree=c(50,500,5000,50000))
fitForest<-randomForest(formula, data=trainFeatForest, nTree=20000)
PredictionForest <- predict(fitForest, as.data.frame(trainFeatForest))
results.matrix <- confusionMatrix((PredictionForest), trainFeatForest$Survived)
accuracyForest<-results.matrix$overall[1]

PredictionForest <- predict(fitForest, newdata=as.data.frame(testFeatForest))
submit <- data.frame(PassengerID = testFeat$PassengerId, Survived = (PredictionForest))
write.csv(submit, file = "submitCJC.csv", row.names = FALSE)
#839   ↓111 	
#Charles Copley
#0.79426 	20 	Mon, 15 Jun 2015 19:17:13 (-9.3d)
#Your Best Entry ↑
#Your submission scored 0.77512, which is not an improvement of your best score. Keep trying! 
#-----------------------------------------------------------------------------------------------------------



#-----------------------------generalized boost regression------------------------------------------------------------------------
formula <-Survived ~  Pclass + Sex + FarePassenger+ Age + Embarked + EthnicFeat + TitleFeat + GroupFeat + MFCoupleFeat + FamilyFeat + CabinFeat
#use generalized boost regression algorithm for classification
fitControl <- trainControl(method = "repeatedcv",number = 10,repeats = 10)
#expand the parameter search grid
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9),
                        n.trees = (1:30)*50,
                        shrinkage = 0.1,
                        n.minobsinnode = 10)
set.seed(1)
gbmFit2 <- train(formula, data = as.data.frame(trainFeat),
                 method = "gbm",
                 trControl = fitControl,
                 verbose = FALSE,
                 ## Now specify the exact models 
                 ## to evaludate:
                 tuneGrid = gbmGrid)
#The final values used for the model were n.trees = 50, interaction.depth = 9, shrinkage = 0.1 and n.minobsinnode = 10. 

fitBoost<-gbm(formula,data= as.data.frame(trainFeat), n.trees=50,interaction.depth=9, shrinkage=0.1,n.minobsinnode=10,distribution="gaussian")
PredictionBoost <- predict(fitBoost, as.data.frame(trainFeat),n.trees=50)
results.matrix <- confusionMatrix(round(PredictionBoost), trainFeat$Survived)
accuracyBoost<-results.matrix$overall[1]

PredictionBoost <- predict(fitBoost, newdata=as.data.frame(testFeat),n.trees=50,interaction.depth=9, shrinkage=0.1,n.minobsinnode=10,distribution="gaussian")
submit <- data.frame(PassengerID = testFeat$PassengerId, Survived = round(PredictionBoost))
write.csv(submit, file = "submitCJC.csv", row.names = FALSE)

#with n.trees = 20000
#837   ↓109 	
#Charles Copley
#0.79426 	19 	Mon, 15 Jun 2015 18:57:13 (-9.3d)
#Your Best Entry ↑
#Your submission scored 0.78469, which is not an improvement of your best score. Keep trying! 

#with n.trees = 50, interaction.depth = 9, shrinkage = 0.1 and n.minobsinnode = 10. 
#842   ↓109 	
#Charles Copley
#0.79426 	22 	Tue, 16 Jun 2015 05:15:55 (-9.7d)
#Your Best Entry ↑
#Your submission scored 0.77512, which is not an improvement of your best score. Keep trying! 
