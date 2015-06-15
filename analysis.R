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


#read in the data
train <- read.csv('train.csv')
test  <-read.csv('test.csv')
testpassengerIDs<-test$PassengerID
#I get the 2010 US census data in an effort to reconstitute the ethnicity of the individual passengers
names  <-read.csv('USCensusNames2000.csv',stringsAsFactors = FALSE)
names[6:11]<-as.numeric(unlist(names[6:11]))

#plotData(train)
#First clean the data somewhat and create a bunch of features.
ageBreaks <- c(0,2,5,10,15,18,30,40,50,60,70,80,90)
priceBreaks <- c(seq(from=0,by=5, to=95),seq(from=100,by=10, to=190),seq(from=200,by=20, to=540))

#clean the data and create new features for the full set.
fullData<-(cleanData(train,test,names,ageBreaks,priceBreaks))
#clean the data and create new features for the full set.
test<-as.data.frame(append(test, list(Survived = NA), after = 1))
fullData2<-rbind(train,test)

fullData$TitleFeat<-as.factor(fullData$TitleFeat)
fullData$GroupFeat<-as.factor(fullData$GroupFeat)
fullData$MFCoupleFeat<-as.factor(fullData$MFCoupleFeat)
fullData$Family<-as.factor(fullData$FamilyFeat)
fullData$CabinFeat<-as.factor(fullData$CabinFeat)


trainFeat <- (fullData[1:891,])
testFeat <- (fullData[892:1309,])




formula <-Survived ~  Pclass + Sex + Fare+ Age + Embarked + EthnicFeat + TitleFeat + GroupFeat + MFCoupleFeat + FamilyFeat + CabinFeat





#lets explore this model a little with recursive partioning.
fit <- rpart(formula, data=as.data.frame(trainFeat), method="class",control=rpart.control(minsplit=50,cp=0))
Prediction <- predict(fit, as.data.frame(trainFeat), type = "class")
results.matrix <- confusionMatrix(Prediction, trainFeat$Survived)
accuracyRpart<-results.matrix$overall[1]
pdf("rTree1.pdf")
fancyRpartPlot(fit)
dev.off()


#find the best SVM gamma and cost parameters
tuned <- tune.svm(formula, data=trainFeat, gamma = 10^(-3:3), cost = 10^(-2:4))
fitSVM  <- svm(formula, data = as.data.frame(trainFeat), 
               type="C-classification",
               kernel="radial",
               probability=T,
               gamma=0.1,
               cost=1) 
testFeat$Survived<-NULL
svmmodel <- predict(fitSVM, as.data.frame(testFeat),probability=T,type="C-classification")



PredictionSVM <- predict(fitSVM, data=as.data.frame(trainFeat), type="C-classification")
results.matrix <- confusionMatrix((PredictionSVM), trainFeat$Survived)
accuracySVM<-results.matrix$overall[1]



PredictionSVM2 <- predict(fitSVM, newdata=as.data.frame(testFeat), type="C-classification")
submit <- data.frame(PassengerID = testFeat$PassengerId, Survived = (PredictionSVM2))
write.csv(submit, file = "submitCJC.csv", row.names = FALSE)




x <- subset(trainFeat, select = -Survived)
y <- trainFeat$Survived
modelSVM<-svm(x,y)

model <- svm(vs ~ hp+mpg+gear,data=mtcars)
predict(model)







fullDataFeature<-as.data.frame(fullData)
fullDataFeature$Survived<-as.numeric(fullDataFeature$Survived)
fullDataFeature$SexFeat<-as.numeric(fullDataFeature$SexFeat)
fullDataFeature$AgeFeat<-as.numeric(fullDataFeature$AgeFeat)
fullDataFeature$PclassFeat<-as.numeric(fullDataFeature$PclassFeat)
fullDataFeature$titleFeat<-as.numeric(fullDataFeature$TitleFeat)
fullDataFeature$CabinFeat<-as.numeric(fullDataFeature$CabinFeat)
fullDataFeature$GroupFeat<-as.numeric(fullDataFeature$GroupFeat)
fullDataFeature$MFCoupleFeat<-as.numeric(fullDataFeature$MFCoupleFeat)
fullDataFeature$FamilyFeat<-as.numeric(fullDataFeature$FamilyFeat)
levels(fullDataFeature$Survived)<-c("0","1")



trainFactor <- fullDataFeature[1:891,]
testFactor <- fullDataFeature[892:1309,]
train1Factor <-fullDataFeature[1:800,]
test1Factor<-fullDataFeature[801:891,]


train <- fullData[1:891,]
test <- fullData[892:1309,]
train1 <-fullData[1:800,]
test1<-fullData[801:891,]


formulaSVM <-(Survived) ~ (PclassFeat)

model  <- svm((Survived) ~ PclassFeat, data = as.data.frame(trainFeat), type = "C-classification",family=binomial) 
PredictionSVM <- predict(model, data=as.data.frame(testFeat), type = "C-classification", na.action=na.fail)


data<-as.data.frame(cbind(trainFactor$Survived,trainFactor$PclassFeat))
dataTest<-as.data.frame(cbind(testFactor$Survived,testFactor$PclassFeat))
colnames(data)<-c("Survived",'PclassFeat')

fitsvm<- svm(formulaSVM,data=data, scale = TRUE)
Predictionsvm <- predict(fitsvm, data=dataTest)

data(iris)
attach(iris)

## classification mode
# default with factor response:
model <- svm(Species ~ ., data = iris)
x <- subset(iris, select = -Species)
y <- Species
model <- svm(x, y) 
# create 2-dim. normal with rho=0:
X <- data.frame(a = rnorm(1000), b = rnorm(1000))
attach(X)

# traditional way:
m <- svm(X, gamma = 0.1)

# formula interface:
m <- svm(~., data = X, gamma = 0.1)
# or:
m <- svm(~ a + b, gamma = 0.1)

# test:
newdata <- data.frame(a = c(0, 4), b = c(0, 4))
predict (m, newdata)

fit <- rpart(formula,data=as.data.frame(train), method="class",control=rpart.control(minsplit=25,cp=0))
fitlm <- glm(formula,data=as.data.frame(train))
fitnnet <- nnet(formula,data=trainFactor,size=10)
tt<-as.data.frame(train)
tt$Survived<-tt$Survived-1
tt$Age<-scale(tt$Age)
tt$Fare<-scale(tt$Fare)
fitBoost<-gbm(formula,data= as.data.frame(tt), n.trees=2000,interaction.depth=2, distribution="gaussian")
fitForest<-randomForest(formula, data=tt, nTree=20000,type-'classification')
fitbayes <- naiveBayes(formula, data = as.data.frame(tt),laplace=3)

fancyRpartPlot(fit)
fit <- kmeans(na.omit(data.frame(train)), 5)


tt<-as.data.frame(test)
tt$Age<-scale(tt$Age)
tt$Fare<-scale(tt$Fare)

Prediction <- predict(fit, as.data.frame(test), type = "class")
Predictionlm <- predict(fitlm, as.data.frame(test),type='response')
Predictionnnet <- predict(fitnnet, testFactor)
PredictionForest <- predict(fitForest, tt)
PredictionBoost <- predict(fitBoost, as.data.frame(tt),n.trees=2000)
PredictionBayes <- predict(fitbayes, as.data.frame(tt))

#tune <- tune.svm(formula, data=as.data.frame(train1), gamma=10^(-6:-1), cost=10^(1:4))


levels(Prediction)<-c("0","1")
levels(Predictionlm)<-c("0","1")
levels(Predictionsvm)<-c("0","1")

(table((Prediction),as.data.frame(test)$Survived))
(table(round(Predictionlm),as.data.frame(test)$Survived))
(table(round(Predictionnnet),as.data.frame(train)$Survived))
(table(round(Predictionsvm),as.data.frame(train)$Survived))
(table((PredictionForest),as.data.frame(testFactor)$Survived))
(table((PredictionBoost),as.data.frame(testFactor)$Survived))


submit <- data.frame(PassengerID = 892:1309, Survived = (PredictionBayes))
write.csv(submit, file = "submitCJC.csv", row.names = FALSE)
