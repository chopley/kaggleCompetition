plotDataFeatures <- function(train){

  #plot survival by fare
aa<-prop.table(table(train$Survived-1,train$PriceFeat),2)
mp<-barplot(aa[2,])

}