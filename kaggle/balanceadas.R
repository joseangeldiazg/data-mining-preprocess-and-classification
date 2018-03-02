## Clases balanceadas

TrainPrueba<-TrainMVBien
TestPrueba<-TestMVBien

#Dividimos los problemas en binarios

table(TrainClean$y)
train0<-TrainClean[which(TrainClean$y=="0"),]
train1<-TrainClean[which(TrainClean$y=="1"),]
train2<-TrainClean[which(TrainClean$y=="2"),]
train3<-TrainClean[which(TrainClean$y=="3"),]

train0vs3 <- rbind(train0,train3)
train1vs3 <- rbind(train1,train3)
train2vs3 <- rbind(train2,train3)


#Ya tenemos los problemas divididos en binarios por lo que ahora aumentaremos las clases minoritarias con smote. 


train0vs3$y <- as.numeric(train0vs3$y)-1
train0vs3$y <- as.factor(train0vs3$y)

train1vs3$y <- as.numeric(train1vs3$y)-1
train1vs3$y <- as.factor(train1vs3$y)

train2vs3$y <- as.numeric(train2vs3$y)-1
train2vs3$y <- as.factor(train2vs3$y)

#Aplciamos el smote para crear las muestras deseadas

table(train0vs3$y)
newTrain0vs3 <- ubSMOTE(y ~.,perc.over = 4500)
table(newTrain0vs3$y)

table(train1vs3$y)
newTrain1vs3 <- ubSMOTE(y ~.,perc.over = 195)
table(newTrain1vs3$y)


table(train2vs3$y)
newTrain2vs3 <- ubSMOTE(y ~.,perc.over = 10)
table(newTrain2vs3$y)




#Ahora cogemos las muestras de las últimas clases generadas y las pasamos al modelo. 

train0<-newTrain0vs3[which(newTrain0vs3$y=="0"),]
train1<-newTrain1vs3[which(newTrain1vs3$y=="1"),]
train2<-newTrain2vs3[which(newTrain2vs3$y=="2"),]

oversampledTrain<-rbind(train0, train1, train2, train3)
oversampledTrain<-oversampledTrain[sample(nrow(oversampledTrain)),]



table(oversampledTrain$y)


out <- IPF(y~., data = oversampledTrain, s = 2)
oversampledTrainClean<-out$cleanData


table(oversampledTrainClean$y)