# Experimento Rpart y Knn con numéricas y valores perdidos con norm y


TrainExperimento <- TrainSinNumericasMVNorm
TrainExperimento$y <- as.factor(TrainSinNumericasMVNorm$y)
TestExperimento <- TestSinNumericasMVNorm


TrainExperimento$x0<- as.factor(TrainExperimento$x0)
TrainExperimento$x0 <- factor( TrainExperimento$x0, levels = c(1,2,3,4,5))


# Aplicamos filtro de ruido


TrainExperimento$y<-as.factor(TrainExperimento$y)
out <- IPF(y~., data = TrainExperimento, s = 2)
TrainExperimento<-out$cleanData


# Aplicamos smote


#Dividimos los problemas en binarios

table(TrainExperimento$y)
train0<-TrainExperimento[which(TrainExperimento$y=="0"),]
train1<-TrainExperimento[which(TrainExperimento$y=="1"),]
train2<-TrainExperimento[which(TrainExperimento$y=="2"),]
train3<-TrainExperimento[which(TrainExperimento$y=="3"),]

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
newTrain0vs3 <- SMOTE(y ~.,train0vs3, perc.over = 2800, perc.under = 100)
table(newTrain0vs3$y)

table(train1vs3$y)
newTrain1vs3 <- SMOTE(y ~.,train1vs3, perc.over = 160, perc.under = 100)
table(newTrain1vs3$y)

table(train2vs3$y)
newTrain2vs3 <- SMOTE(y ~.,train1vs3, perc.over = 160, perc.under = 100)
table(newTrain2vs3$y)

#Ahora cogemos las muestras de las últimas clases generadas y las pasamos al modelo. 

train0<-newTrain0vs3[which(newTrain0vs3$y=="0"),]
train1<-newTrain1vs3[which(newTrain1vs3$y=="1"),]

TrainExperimento<-rbind(train0, train1, train2, train3)
TrainExperimento<-TrainExperimento[sample(nrow(TrainExperimento)),]
table(TrainExperimento$y)


#Quitamos algunas

TrainExperimento$y<-as.factor(TrainExperimento$y)
out <- IPF(y~., data = TrainExperimento, s = 2)
TrainExperimento<-out$cleanData


#Creamos el modelo


control <- trainControl(method = "cv", number = 10, returnResamp = "all", search = "random")

trainModelRpart <- caret::train(y ~.,  data = TrainExperimento, 
                         method = "rpart", 
                         trControl = control,
                         preProc = c("center", "scale"),
                         tuneLength = 50)

trainModelRpart

trainModelKNN <- caret::train(y ~.,  data = TrainExperimento, 
                       method = "knn",
                       trControl = control,
                       preProc = c("center", "scale"),
                       tuneGrid = data.frame(.k=1))


trainModelKNN



TrainExperimento[which(is.na(TrainExperimento)),] <-3

testing <- predict(trainModelRpart, TestExperimento)

testing <- cbind(Id=1:683, Prediction=as.integer(as.character(testing)))
write.table(testing, "sub/rpart-norm-ipf-smote-ipf.csv", col.names = TRUE, row.names = FALSE,
            quote=FALSE, sep = ",")
