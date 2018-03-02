#José Ángel

#**********************************************
#Experimento con seleccion de variables usando boruta
#**********************************************

#Librerias
library(Boruta)
library(caret)
library(NoiseFiltersR)


#**********************************************
# Carga de datos
#**********************************************

Train <- read.csv2("data/Numericos_ImpKNN_SinCorr-TRAIN.csv", stringsAsFactors = FALSE)
Test <- read.csv2("data/Numericos_ImpKNN_SinCorr-TEST.csv", stringsAsFactors = FALSE)

# Las copio a variables nuevas para trabajar sobre ellas

train <- Train
test <- Test

# Comprobación de NAs

sum(is.na(train))
sum(is.na(test))

(table(train[,75])/dim(train)[1])*100 # Clase 0 desbalanceada


#**********************************************
# Selección de variables más relevantes
#**********************************************


Boruta.train <- Boruta(y ~ ., data = train, doTrace = 2, ntree = 500)
Boruta.train
# Creamos un gráfico par ver como se ha comportado el proceso
plot(Boruta.train)

# Obtenemos las variables consideradas relevantes

getConfirmedFormula(Boruta.train)
variables.relevantes<-which(Boruta.train$finalDecision=="Confirmed")

trainEntrenar<-train[,c(variables.relevantes,75)]
testPredecir<-test[,c(variables.relevantes)]


#**********************************************
# Aumentamos la clase minoritaria
#**********************************************

(table(trainEntrenar[,56])/dim(trainEntrenar)[1])*100


trainEntrenar <- rbind(trainEntrenar, trainEntrenar[which(trainEntrenar[,56]==0),]) # 0.84
trainEntrenar <- rbind(trainEntrenar, trainEntrenar[sample(which(trainEntrenar[,56]==1), length(which(trainEntrenar[,56]==1))/2.5),]) 


(table(trainEntrenar[,56])/dim(trainEntrenar)[1])*100


#**********************************************
# Aplicamos ruido de nuevo
#**********************************************
#trainEntrenar$y<-as.factor(trainEntrenar$y)
#out <- IPF(y~., data = trainEntrenar, s = 2)
#trainEntrenar<-out$cleanData
#(table(trainEntrenar[,56])/dim(trainEntrenar)[1])*100


#**********************************************
# Entrenamos el modelo
#**********************************************

modeloCaret0 <- caret::train(trainEntrenar[,-c(56)], as.factor(trainEntrenar[,56]), method="knn",
                             tuneGrid = data.frame(k=1),
                             preProcess = c("center", "scale"),
                             tuneLength = 10,
                             trControl = trainControl(method = "cv"))


#Acc en training 0.8837173

#**********************************************
# Predecimos y creamos la salida
#**********************************************

testing <- predict(modeloCaret0, testPredecir)

predicciones <- as.integer(as.character(testing))

# Construyo la matriz de predicciones
predicciones <- matrix(c(1:683,predicciones),683,2,byrow = FALSE)
colnames(predicciones) <- c("Id","Prediction")
# Guardo
write.table(predicciones, "sub/num-enn-ipf-borouta-ros.csv", col.names = TRUE, row.names = FALSE,
            quote=FALSE, sep = ",")
