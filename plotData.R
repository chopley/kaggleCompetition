plotData <- function(train){
  
  
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
  
  
}