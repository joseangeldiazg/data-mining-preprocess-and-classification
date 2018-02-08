#Dummificar Variables


TrainDummi <- read.csv2("data/my_dataset_train.csv", sep=",", dec=".", stringsAsFactors = FALSE)
TestDummi <- read.csv2("data/my_dataset_test.csv", sep=",", dec=".", stringsAsFactors = FALSE)


#Imputamos valores perdidos

mice_mod <- mice(TrainDummi[,colnames(TrainDummi)], method='norm')
TrainDummi <- complete(mice_mod)
summary(TrainDummi)


mice_mod <- mice(TestDummi[,colnames(TestDummi)], method='norm')
TestDummi <- complete(mice_mod)
summary(TestDummi)

#Para las variables categóricas usamos otro modelo

mice_mod <- mice(TrainDummi[,colnames(TrainDummi)], method='pmm')
TrainDummi <- complete(mice_mod)
summary(TrainDummi)


mice_mod <- mice(TestDummi[,colnames(TestDummi)], method='pmm')
TestDummi <- complete(mice_mod)
summary(TestDummi)

#Obtememos las variables categóricas ylas dumificamos


head(TrainDummi[,c("x0", "x14", "x17", "x51", "x61", "x63")])



dummyx1<-dummy(TrainDummi$x0)
dummyx14<-dummy(TrainDummi$x14)
dummyx17<-dummy(TrainDummi$x17)
dummyx51<-dummy(TrainDummi$x51)
dummyx61<-dummy(TrainDummi$x61)
dummyx63<-dummy(TrainDummi$x63)

dummisandclass<-cbind(as.factor(dummyx1[,-1]),as.factor(dummyx14[,-1]),as.factor(dummyx17[,-1]),
                      as.factor(dummyx51[,-1]),as.factor(dummyx61[,-1]),as.factor(dummyx63[,-1]),y=TrainDummi$y)

TrainDummi<-cbind(TrainDummi[,-c(1,15,18,52,62,64,76)],dummisandclass)



#Aplicamos lo mismo a test


dummyx1<-dummy(TestDummi$x0)
dummyx14<-dummy(TestDummi$x14)
dummyx17<-dummy(TestDummi$x17)
dummyx51<-dummy(TestDummi$x51)
dummyx61<-dummy(TestDummi$x61)
dummyx63<-dummy(TestDummi$x63)


dummisandclass<-cbind(as.factor(dummyx1[,-1]),as.factor(dummyx14[,-1]),as.factor(dummyx17[,-1]),as.factor(dummyx51[,-1]),as.factor(dummyx61[,-1]),as.factor(dummyx63[,-1]))

TestDummi<-cbind(TestDummi[,-c(1,15,18,52,62,64)],dummisandclass)

describe(TestDummi)

#Vamos aplicar filtros de ruido

TrainDummi$y<- as.factor(TrainDummi$y)
out <- IPF(y~., data = TrainDummi, s = 2)
TrainCleanDummi<-out$cleanData




#Vamos a aplicar SMOTE



#Dividimos los problemas en binarios

table(TrainCleanDummi$y)
train0<-TrainCleanDummi[which(TrainCleanDummi$y=="0"),]
train1<-TrainCleanDummi[which(TrainCleanDummi$y=="1"),]
train2<-TrainCleanDummi[which(TrainCleanDummi$y=="2"),]
train3<-TrainCleanDummi[which(TrainCleanDummi$y=="3"),]

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
newTrain0vs3 <- SMOTE(y ~., train0vs3,perc.under = 97  ,perc.over = 5500)
table(newTrain0vs3$y)

table(train1vs3$y)
newTrain1vs3 <- SMOTE(y ~., train1vs3,perc.under = 360  ,perc.over = 195)
table(newTrain1vs3$y)


table(train2vs3$y)
newTrain2vs3 <- SMOTE(y ~., train2vs3,perc.under = 200  ,perc.over = 10)
table(newTrain2vs3$y)



#Ahora cogemos las muestras de las últimas clases generadas y las pasamos al modelo. 

train0<-newTrain0vs3[which(newTrain0vs3$y=="0"),]
train1<-newTrain1vs3[which(newTrain1vs3$y=="1"),]
train2<-newTrain2vs3[which(newTrain2vs3$y=="2"),]

oversampledTrainDummi<-rbind(train0, train1, train2, train3)
oversampledTrainDummi<-oversampledTrainDummi[sample(nrow(oversampledTrain)),]



table(oversampledTrainDummi$y)


#Por ultimo aplicamos un filtro de ruido


out <- IPF(y~., data = oversampledTrainDummi, s = 2)
oversampledTrainDummiClean<-out$cleanData


table(oversampledTrainDummiClean$y)



#Vamos a entrenar el modelo a ver que resultados ofrece



control <- trainControl(method = "cv", number = 10, returnResamp = "all", search = "random")

trainModelRpart <- train(y ~.,  data = oversampledTrainDummiClean, 
                         method = "rpart", 
                         trControl = control,
                         preProc = c("center", "scale"),
                         tuneLength = 50)

trainModelRpart

trainModelKNN <- train(y ~.,  data = oversampledTrainDummi, 
                       method = "knn",
                       trControl = control,
                       preProc = c("center", "scale"),
                       tuneGrid = data.frame(.k=1))


trainModelKNN

#Train dummi con mv -> 0.6664229
#Train dummi con mv ipf -> 0.78





