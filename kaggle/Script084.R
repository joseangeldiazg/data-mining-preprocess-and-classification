# Carga de datos
Train <- read.csv2("data/Numericos_ImpKNN_SinCorr-TRAIN.csv", stringsAsFactors = FALSE)
Test <- read.csv2("data/Numericos_ImpKNN_SinCorr-TEST.csv", stringsAsFactors = FALSE)

# Las copio a variables nuevas para trabajar sobre ellas
train <- Train
test <- Test

# Comprobación de NAs
sum(is.na(train))
sum(is.na(test))

(table(train[,75])/dim(train)[1])*100 # Clase 0 desbalanceada

# Selección de variables más relevantes
library(randomForest)
rm <-randomForest::randomForest(train[,-c(75)], as.factor(train[,75]))
importance <- caret::varImp(rm, scale=FALSE)
var<-order(importance[[1]])
var[1:25] # Voy a descartar estas variables

# SMOTE
#train0 <- train
#train0[which(train0[,75]%in%c(1,2,3)),75]<-1
#train0[,75] <- factor(train0[,75])
#(table(train0[,75])/dim(train0)[1])*100

library(unbalanced)
#smote <- unbalanced::ubSMOTE(train0[,-75], train0[,75], perc.over = 100, perc.under = 100)
#smote <- smote$X[which(smote$Y==1),] # Cojo los sinteticos de la clase minoritaria
#smote <- cbind(smote, y=0)
#train <- rbind(train, smote)

library(smotefamily)
#smote <- smotefamily::SMOTE(train0[,-75], train0[,75], dup_size = 1.8)
#smote <- smote$syn_data
#names(smote)[75] <- "y"
#train <- rbind(train, smote)


train <- Train
(table(train[,75])/dim(train)[1])*100
train <- rbind(train, train[which(train[,75]==0),]) # 0.84
train <- rbind(train, train[sample(which(train[,75]==1), length(which(train[,75]==1))/2.5),]) 
#KNN
library(caret)
modeloCaret0 <- caret::train(train[,-c(var[1:25], 75)], as.factor(train[,75]), method="knn",
                             tuneGrid = data.frame(k=1),
                             preProcess = c("center", "scale"),
                             tuneLength = 10,
                             trControl = trainControl(method = "cv"))
modelo.predict0 <- caret::predict.train(modeloCaret0, Train, type="raw")
predicciones <- as.integer(as.character(modelo.predict0))


mean(predicciones==Train[,75]) # 1
modeloCaret0 





# Construyo la matriz de predicciones
predicciones <- matrix(c(1:683,predicciones),683,2,byrow = FALSE)
colnames(predicciones) <- c("Id","Prediction")
# Guardo
write.table(predicciones, "sample.csv", col.names = TRUE, row.names = FALSE,
            quote=FALSE, sep = ",")
