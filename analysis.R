library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(nnet)
library(MASS)
library(e1071)

#read in the data
train <- read.csv('train.csv')
test  <-read.csv('test.csv')
names  <-read.csv('app_c.csv',stringsAsFactors = FALSE)
names[6:11]<-as.numeric(unlist(names[6:11]))
#First clean the data somewhat


test$Survived <- NA
fullData <- rbind(train, test)
fullData$Name <- as.character(fullData$Name)
fullData$Title <- sapply(fullData$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
fullData$Surname <- sapply(fullData$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][1]})


#lets try and get ethnicity of the passengers using the US 2000 census data
surname<-NA
missedSurname<-NA
fullData$SurnameStrip<-toupper(fullData$Surname)
fullData$SurnameStrip<-sub("'","",fullData$SurnameStrip)
listOfSurnames<-unique(fullData$SurnameStrip)
for(i in 1:length(fullData$Surname)){
#for(i in 1:100){
    nameinCensus<-which(listOfSurnames[i]==names$name)
    if(length(nameinCensus)==0){
      missedSurname<-rbind(missedSurname,listOfSurnames[i])
    }
    surname<-rbind(surname,names[nameinCensus,])
}

asian<-surname$name[which(surname$pctapi>10)]
white<-surname$name[which(surname$pctwhite>30)]
hispanic<-surname$name[which(surname$pcthispanic>20)]

for(i in 1:length(fullData$SurnameStrip)){
  if(sum(fullData$SurnameStrip[i]==asian)>0){
    fullData$EthnicFeat[i]<-'asian'
  }
  else if(sum(fullData$SurnameStrip[i]==hispanic)>0){
    fullData$EthnicFeat[i]<-'hispanic'
  }
  else if(sum(fullData$SurnameStrip[i]==white)>0){
    fullData$EthnicFeat[i]<-'white'
  }
  else{
    fullData$EthnicFeat[i]<-'Unknown'
  }

}

#Now we split the data up using the ticket numbers. Each ticket number corresponds to one group travelling together.
ticketSplit<-split(fullData,fullData$Ticket)
fullData$GroupFeat=NA
for(i in 1:length(ticketSplit))
{
  fullData$GroupFeat[ticketSplit[i][[1]][1]$PassengerId] = length(ticketSplit[i][[1]][1]$PassengerId)
}


#create a feature if it is a single couple travelling together
fullData$MFCoupleFeat<-(fullData$GroupFeat==2&fullData$SibSp==1&fullData$Parch==0)
#create a feature for a family travelling together
fullData$FamilyFeat<-(fullData$GroupFeat>=3&fullData$Parch>=1)
fullData$Surname[which(fullData$Surname==fullData$Surname[fullData$MFCoupleFeat][1])]

#extract the titles of the passengers using strsplit
fullData$Title <- sapply(fullData$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
#and get rid of the extra space
fullData$Title <- sub(' ', '', fullData$Title)

#lets make a few estimates for the ages- We'll use the persons title as a proxy and replace the values of 0 and Na with the average for that class
titles<-unique(fullData$Title)
ages<-sapply(1:17,FUN=function(x) {mean(na.omit(fullData$Age[fullData$Title==titles[x]]))})
for(i in 1:17)
{
  fullData$Age[which(is.na(fullData$Age)&fullData$Title==titles[i])]<-ages[i]
}

#get average Fares in the different classes
averageFares<-sapply(1:3, FUN=function(x) {mean(na.omit(fullData$Fare[fullData$Pclass==x]))})
#replace fares with average of each class?
for(i in 1:3)
{
  fullData$Fare[which(((fullData$Fare==0)|is.na(fullData$Fare))&fullData$Pclass==i)]<-averageFares[i]
}

#lets try and classify the titles into either people that might be more likely to survive (i.e. rich, or respected) and those less likely to survive
unique(fullData$Title)
#not a clue what Jonkeer is?
jonkheerIndex <-grep('Jonkheer',fullData$Title)
fullData$Name[jonkheerIndex]
fullData$Age[jonkheerIndex]
fullData$Pclass[jonkheerIndex]
#ok a Jonkheer is a  middle-aged man travelling in first class.
fullData$TitleFeat<-fullData$Title
#titles that appear more likely to survive than an average male
fullData$TitleFeat[fullData$Title %in% c('Major', 'Sir','Dr','Col')] <- 'GoodMale'
fullData$TitleFeat[fullData$Title %in% c('Master')] <- 'ChildMale'
fullData$TitleFeat[fullData$Title %in% c('Jonkheer', 'Mr','Don')] <- 'Male'
#titles that appear less likely to survive than an average male
fullData$TitleFeat[fullData$Title %in% c('Rev','Capt')] <- 'vBadMale'
#titles that appear more likely to survive than an average female
fullData$TitleFeat[fullData$Title %in% c('the Countess', 'Mme', 'Mlle','Lady','Ms')] <- 'vGoodFemale'
fullData$TitleFeat[fullData$Title %in% c('Mrs','Dona')] <- 'GoodFemale'
#titles that appear less likely to survive than an average female
fullData$TitleFeat[fullData$Title %in% c('Miss')] <- 'BadFemale'
prop.table(table(fullData$TitleFeat,fullData$Survived),1)
fullData$TitleFeat=factor(fullData$TitleFeat)

#lets look at the cabin information? I'd magine the reason that there aren't fully sampled cabins is because this information was gotten from survivors?
#First lets just separate by floor
fullData$Cabin <-as.character(fullData$Cabin)
fullData$CabinFeat <- sapply(fullData$Cabin, FUN=function(x) {strsplit(x, split='[ ]')[[1]][1]})
fullData$CabinFeat<-sub("A[0-9]+","A",fullData$CabinFeat)
fullData$CabinFeat<-sub("B[0-9]+","B",fullData$CabinFeat)
fullData$CabinFeat<-sub("C[0-9]+","C",fullData$CabinFeat)
fullData$CabinFeat<-sub("D[0-9]+","D",fullData$CabinFeat)
fullData$CabinFeat<-sub("E[0-9]+","E",fullData$CabinFeat)
fullData$CabinFeat<-sub("F[0-9]+","F",fullData$CabinFeat)
fullData$CabinFeat<-sub("G[0-9]+","G",fullData$CabinFeat)
fullData$CabinFeat[is.na(fullData$CabinFeat)]<-'Unknown'





#lets look at percentage that survived vs not
pdf("Survival.pdf")
par(mar=c(0,1,3,1)+0.1)
n<-length(train$Sex)
title<-sprintf("Aggregate Survival in Training Data Set\nn=%d",n)
prop.table(table(train$Survived))
percentlabels<- round(100*prop.table(table(train$Survived))/sum(prop.table(table(train$Survived))), 1)
pielabels<- paste(percentlabels, "%", sep="")
colsStandard<-c("white","grey90")
pie(prop.table(table(train$Survived)),main=title,labels=pielabels,col=colsStandard,cex=1)
legend("topleft", c("Perished","Survived"),fill=colsStandard,cex=1,pt.cex=1.5)
dev.off()

#Lets divide the ages into groups for later analysis
ageBreaks <- c(0,2,5,10,15,18,30,40,50,60,70,80,90)
train$AgeFeat<-cut(train$Age,ageBreaks,labels=FALSE)
fullData$AgeFeat<-cut(fullData$Age,ageBreaks,labels=FALSE)
#plot the women survival
pdf("SurvivalBySexFemale.pdf")
par(mar=c(0,1,3,1)+0.1)
female<-split(train,train$Sex=='female')
n<-length(which(train$Sex=='female'))
title<-sprintf("Woman Aggregate Survival\nn=%d",n)
percentlabels<- round(100*prop.table(table(female$'TRUE'$Survived)), 1)
pielabels<- paste(percentlabels, "%", sep="")
pie(prop.table(table(female$'TRUE'$Survived)),main=title,labels=pielabels,col=colsStandard,cex=1)
legend("topleft", c("Perished","Survived"),fill=colsStandard,cex=1,pt.cex=1.5)
dev.off()

#plot the male aggregate survival
pdf("SurvivalBySexMale.pdf")
par(mar=c(0,1,3,1)+0.1)
male<-split(train,train$Sex=='male')
n<-length(which(train$Sex=='male'))
title<-sprintf("Men Aggregate Survival\nn=%d",n)
percentlabels<- round(100*prop.table(table(male$'TRUE'$Survived)), 1)
pielabels<- paste(percentlabels, "%", sep="")
pie(prop.table(table(male$'TRUE'$Survived)),main=title,labels=pielabels,col=colsStandard,cex=1)
legend("topleft", c("Perished","Survived"),fill=colsStandard,cex=1,pt.cex=1.5)
dev.off()


#plot the male survival by class
for (i in 1:3){
  filename<-sprintf("MaleSurvival_Class=%d.pdf",i)
  pdf(filename)
  n<-length(which(male$'TRUE'$Pclass==i))
  title<-sprintf("Male Survival Class=%d\nn=%d",i,n)
  percentlabels<- round(100*prop.table(table(male$'TRUE'$Survived,male$'TRUE'$Pclass),2)[,i])
  pielabels<- paste(percentlabels, "%", sep="")
  pie((table(male$'TRUE'$Survived,male$'TRUE'$Pclass))[,i],label=pielabels,main=title,col=colsStandard,cex=1)
  #dev.copy(png,filename)
  legend("topleft", c("Perished","Survived"),fill=colsStandard,cex=1,pt.cex=1.5)
  dev.off()
}

#plot the female survival by class
for (i in 1:3){
  filename<-sprintf("FemaleSurvival_Class=%d.pdf",i)
  pdf(filename)
  n<-length(which(female$'TRUE'$Pclass==i))
  title<-sprintf("Female Survival Class=%d\nn=%d",i,n)
  filename<-sprintf("FemaleSurvival_Class=%d.png",i)
  percentlabels<- round(100*prop.table(table(female$'TRUE'$Survived,female$'TRUE'$Pclass),2)[,i])
  pielabels<- paste(percentlabels, "%", sep="")
  pie((table(female$'TRUE'$Survived,female$'TRUE'$Pclass))[,i],label=pielabels,main=title,col=colsStandard,cex=1)
  #dev.copy(png,filename)
  legend("topleft", c("Perished","Survived"),fill=colsStandard,cex=1,pt.cex=1.5)
  dev.off()
}




#plot the survival by age
for (i in 2:length(ageBreaks)){
  if(length(which(train$AgeFeat==i))>0){
    n<-length(which(train$AgeFeat==i))
    title<-sprintf("Survival Rate Age\n%d-%d Years\nn=%d",ageBreaks[i-1],ageBreaks[i],n)
    filename<-sprintf("Survival_Age=%d_%d.pdf",ageBreaks[i-1],ageBreaks[i])
    pdf(filename)
    percentlabels<- round(100*prop.table(table(train$Survived,train$AgeFeat),2)[,i])
    pielabels<- paste(percentlabels, "%", sep="")
    pie((table(train$Survived,train$AgeFeat))[,i],label=pielabels,main=title,col=colsStandard,cex=1)
    #dev.copy(png,filename)
    legend("topleft", c("Perished","Survived"),fill=colsStandard,cex=1,pt.cex=1.5)
    dev.off()
    
  }
  if(length(which(male$'TRUE'$AgeFeat==i))>0){
    n<-length(which(male$'TRUE'$AgeFeat==i))
    title<-sprintf("Survival Rate Male Age\n%d-%d Years\nn=%d",ageBreaks[i-1],ageBreaks[i],n)
    filename<-sprintf("Survival_Male_Age=%d_%d.pdf",ageBreaks[i-1],ageBreaks[i])
    pdf(filename)
    percentlabels<- round(100*prop.table(table(male$'TRUE'$Survived,male$'TRUE'$AgeFeat),2)[,i])
    pielabels<- paste(percentlabels, "%", sep="")
    pie((table(male$'TRUE'$Survived,male$'TRUE'$AgeFeat))[,i],label=pielabels,main=title,col=colsStandard,cex=1)
    legend("topleft", c("Perished","Survived"),fill=colsStandard,cex=1,pt.cex=1.5)
    #dev.copy(png,filename)
    dev.off()
    
  }
  if(length(which(female$'TRUE'$AgeFeat==i))>0){
    n<-length(which(female$'TRUE'$AgeFeat==i))
    title<-sprintf("Survival Rate Female Age\n%d-%d Years\nn=%d",ageBreaks[i-1],ageBreaks[i],n)
    filename<-sprintf("Survival_Female_Age=%d_%d.pdf",ageBreaks[i-1],ageBreaks[i])
    pdf(filename)
    percentlabels<- round(100*prop.table(table(female$'TRUE'$Survived,female$'TRUE'$AgeFeat),2)[,i])
    pielabels<- paste(percentlabels, "%", sep="")
    pie((table(female$'TRUE'$Survived,female$'TRUE'$AgeFeat))[,i],label=pielabels,main=title,col=colsStandard,cex=1)
   # pie((table(female$'TRUE'$Survived,female$'TRUE'$AgeFeat))[,i],label=c("Perished","Survived"),main=title)
   # dev.copy(png,filename)
    legend("topleft", c("Perished","Survived"),fill=colsStandard,cex=1,pt.cex=1.5)
    dev.off()
  }
}




fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")
fitlm <- glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train)


fancyRpartPlot(fit)












fullData$Survived<-as.factor(fullData$Survived)
fullData$Pclass<-as.factor(fullData$Pclass)
fullData$AgeFeat<-as.factor(fullData$AgeFeat)
fullData$TitleFeat<-as.factor(fullData$TitleFeat)
fullData$CabinFeat<-as.factor(fullData$CabinFeat)
fullData$GroupFeat<-as.factor(fullData$GroupFeat)
fullData$MFCoupleFeat<-as.factor(fullData$MFCoupleFeat)
fullData$FamilyFeat<-as.factor(fullData$FamilyFeat)


train <- fullData[1:891,]
test <- fullData[892:1309,]

train1 <-fullData[1:800,]
test1<-fullData[801:891,]
formula <-as.factor(Survived) ~ Pclass + Sex + as.factor(AgeFeat) + TitleFeat + as.factor(CabinFeat)+as.factor(GroupFeat)+as.factor(MFCoupleFeat)+as.factor(FamilyFeat)+SibSp+Parch
formulaLM <-(Survived) ~ as.numeric((Pclass)) + Sex + Age + SibSp+Parch +TitleFeat + CabinFeat + GroupFeat + MFCoupleFeat + FamilyFeat
formulaSVM <-as.factor(Survived) ~ as.factor(Pclass) + as.factor(Sex)

fit <- rpart(formula,data=train, method="class",control=rpart.control(minsplit=50,cp=0))
fitlm <- glm(formulaLM,data=train)
fitnnet <- nnet(formula,data=train,size=10)
fitsvm<- svm(formulaSVM,data=data.frame(train),na.action= na.omit, scale = TRUE)


fancyRpartPlot(fit)

test$Pclass<-as.factor(test$Pclass)
Prediction <- predict(fit, test, type = "class")
Predictionlm <- predict(fitlm, test)
Predictionnnet <- predict(fitnnet, test)
Predictionsvm <- predict(fitsvm, data.frame(test))
PredictionCombined<-round((as.numeric(Prediction)+round(Predictionlm)+round(Predictionnnet)+(as.numeric((Predictionsvm))-1))/4)

(table((Prediction),test1$Survived))
(table(round(Predictionlm),test1$Survived))

(table(round(Predictionnnet),test1$Survived))
(table((Predictionsvm),test1$Survived))
(table((PredictionCombined),test1$Survived))






submit <- data.frame(PassengerId = test$PassengerId, Survived = round(Predictionnnet))
write.csv(submit, file = "submitCJC.csv", row.names = FALSE)
