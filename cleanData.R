#function that cleans the data, and adds a few features for the learning and partitioning algorithms later on
cleanData <- function(train,test,names,ageBreaks ){

  
  
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
#titles that appear less likely to survive than an average male- Just judging by looking quickly at the data.
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


train$AgeFeat<-cut(train$Age,ageBreaks,labels=FALSE)
fullData2<-data.frame()
ageFeat<-cut(fullData$Age,ageBreaks,labels=FALSE)
fullData2<-cbind(as.factor(fullData$Survived),as.factor(fullData$Sex),as.factor(ageFeat),as.factor(fullData$Pclass),as.factor(fullData$TitleFeat),as.factor(fullData$CabinFeat),as.factor(fullData$GroupFeat),as.factor(fullData$MFCoupleFeat),as.factor(fullData$FamilyFeat))
colnames(fullData2)<-c("Survived","SexFeat","AgeFeat","PclassFeat","TitleFeat","CabinFeat","GroupFeat","MFCoupleFeat","FamilyFeat")



return(((fullData2)))
}