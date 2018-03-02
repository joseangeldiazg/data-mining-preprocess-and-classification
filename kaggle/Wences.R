# Wences

# setwd("/Users/wences/Desktop/Máster/Minería de datos Preprocesamiento y clasificación/Final/MdDPyC/glm")

# Carga de datos
Train <- read.table("data/my_dataset_train.csv", header=TRUE, sep=",", dec=".", stringsAsFactors = FALSE)
Test <- read.table("data/my_dataset_test.csv", header=TRUE, sep=",", dec=".", stringsAsFactors = FALSE)

# Las copio a variables nuevas para trabajar sobre ellas
train <- Train
test <- Test

# Comprobación de valores perdidos mediante una función
valoresPerdidos <- function(y){
  #' Dada una matriz o dataframe se le aplican don sapply, uno por fila y otro por columna
  #' cada uno cuenta los NAs y campos vacios en el caso de caracteres ("")
  #' Tras ello se devuelve una lista con el máximo para cada uno de ellos, el total de
  #' valores perdidos en general y por por columna y los índice de filas y columnas con valores perdidos
  
  instanciasPerdidos <- sapply(1:dim(y)[1], function(x) sum(is.na(y[x,]))+sum(y[x,]=="", na.rm = TRUE))
  
  variablesPerdidos <- sapply(1:dim(y)[2], function(x) sum(is.na(y[,x]))+sum(y[,x]=="", na.rm = TRUE))
  names(variablesPerdidos) <- colnames(y)
  
  return(list("Total perdidos"=sum(instanciasPerdidos),
              "Maximo instancia"=max(instanciasPerdidos),
              "Maximo variable"=max(variablesPerdidos),
              "Total de perdidos por variable"=variablesPerdidos,
              "Instancias con perdidos"=which(instanciasPerdidos!=0),
              "Variables con perdidos"=which(variablesPerdidos!=0)))
}

# Compruebo los valores perdidos mediante mi función
perdidosTrain <- valoresPerdidos(train)
perdidosTest <- valoresPerdidos(test)

# 269 valores perdidos (NA o ""), cada instancia tiene un máximo de 1 y el máximo
# de una variable son 8 (x9)
perdidosTrain$`Total perdidos`
perdidosTrain$`Maximo instancia`
perdidosTrain$`Maximo variable`
perdidosTrain$`Total de perdidos por variable`

# Gráficos perdidos en Train y Test
plot(perdidosTrain$`Total de perdidos por variable`)
plot(perdidosTest$`Total de perdidos por variable`)

# Comparativa de diferencias de variables entre train y test
#diferenciasValoresPerdidos <- abs(perdidosTrain$`Total de perdidos por variable`[-76]-perdidosTest$`Total de perdidos por variable`)
#indiceDiferenciasMinimas <- which(diferenciasValoresPerdidos<2 & perdidosTest$`Total de perdidos por variable`<2 & perdidosTest$`Total de perdidos por variable`>0)
#perdidosTest$`Total de perdidos por variable`[indiceDiferenciasMinimas] #x3, x18, x51, x60, x72 

# Representación de valores perdidos por variables entre test y train
library(ggplot2)
ggplot2::ggplot()+
  geom_col(aes(1:75,perdidosTrain$`Total de perdidos por variable`[-76]),alpha=.8,fill='pink',color='red')+
  geom_col(aes(1:75,perdidosTest$`Total de perdidos por variable`),alpha=.3,fill='lightblue',color='lightblue4')

#################################
# Comprobación de correlaciones #
#################################

variablesNumericas <- sapply(1:dim(train)[2], function(x) is.numeric(train[,x]))
correlaciones <- cor(train[-perdidosTrain$`Instancias con perdidos`,variablesNumericas])
correlacionesFuertes <- sapply(1:dim(correlaciones)[2], function(x) any(correlaciones[-x,x]>0.4 | correlaciones[-x,x]<(-0.4) ))
library(corrplot)
corrplot::corrplot(correlaciones[correlacionesFuertes,correlacionesFuertes], type="lower", diag = FALSE)

###############################
# Correlación entre x41 y x48 #
###############################

# Relleno los NAs de x41 y x48 ya que son variables correlacionadas
dif<-train[1,42]/train[1,49]
ins <- which(is.na(train[,42]))
train[ins,42] <- dif*train[ins,49]

dif<-test[1,42]/test[1,49]
ins <- which(is.na(test[,42]))
test[ins,42] <- dif*test[ins,49]

# Elimino la x48 ya que con una de las dos es suficiente
train <- train[,-49]
test <- test[,-49]

#######################
# Clase desbalanceada #
#######################

# Gráfico que muestra cómo la clase 0 del conjunto de train está muy desbalanceada respecto al resto
ggplot2::ggplot()+
  geom_bar(aes(train[,76]), alpha=.8, fill='lightblue',color='pink') + 
  geom_hline(yintercept = length(train[,76])*0.25, alpha=.5, color='red') +
  geom_text(aes(0, sum(train[,76]==0), label = paste(round(sum(train[,76]==0)/length(train[,76])*100, 2), "%"), vjust = -0.2)) +
  geom_text(aes(1, sum(train[,76]==1), label = paste(round(sum(train[,76]==1)/length(train[,76])*100, 2), "%"), vjust = -0.2)) +
  geom_text(aes(2, sum(train[,76]==2), label = paste(round(sum(train[,76]==2)/length(train[,76])*100, 2), "%"), vjust = -0.2)) +
  geom_text(aes(3, sum(train[,76]==3), label = paste(round(sum(train[,76]==3)/length(train[,76])*100, 2), "%"), vjust = -0.2)) +
  geom_text(aes(0, length(train[,76])*0.25, label = "25 %", vjust = -0.2), color = "red")
  

  
train[,75]

##################
# Imputación KNN #
##################

# Hago la imputación mediante KNN, anteriormente probe con otra que aplicaba la media
# en valores numéricos y la moda en caracteres

imputacionKnnTotal <- function(x){
  #' Se incluye dentro una primera función imputacionKnn
  
  imputacionKnn <- function(x, y){
    #' Se le pasa una matriz o data.frame x y el índice de una de sus columnas
    #' Busca en la columna las instancias perdidas y para esas las variables que tienen valores perdidos de cara 
    #' a que el test no de fallos ya que como no hay más de un valor perdido por instancia será como
    #' máximo el mismo número de instancias perdidas.
    #' Se buscan las instancias con valores perdidos para dichas variables y se omiten para el train,
    #' estando entre ellas evidentemente las que vamos a predecir.
    #' Se construyen train y test y se entrena con K-NN, usando CV y 10 repeticiones, el resultado
    #' es la matriz o data.frame original pero con dicha columna modificada.
    
    require(caret)
    
    # Instancia perdida de la columna
    instanciasPerdidas <- which(is.na(x[,y])|x[,y]=="")
    # Otras variables con datos perdidos en dichas instancias
    variablesPerdidas <- which(sapply((1:dim(x)[2])[-y], function(z) any(is.na(x[instanciasPerdidas,z]) | x[instanciasPerdidas,z]=="")))
    
    # Búsqueda de instancias con perdidos obviando, en caso de que estén, aquellas variables descartadas
    if(length(variablesPerdidas)!=0){
      instanciasX <- sapply(1:dim(x)[1], function(z) sum(is.na(x[z,-variablesPerdidas]))+sum(x[z,-variablesPerdidas]=="", na.rm = TRUE))
    } else {
      instanciasX <- sapply(1:dim(x)[1], function(z) sum(is.na(x[z,]))+sum(x[z,]=="", na.rm = TRUE))
    }
    
    # Quedarme con los índices de las instancias con perdidos
    instanciasX <- which(instanciasX!=0)
    
    if(length(variablesPerdidas)!=0){
      train <- x[-instanciasX, -c(y,variablesPerdidas)]
      test <- x[instanciasPerdidas, -c(y,variablesPerdidas)]
    } else {
      train <- x[-instanciasX,-y]
      test <- x[instanciasPerdidas,-y]
    }
    
    train.class <- x[-instanciasX,y]
    
    
    variablesNumericas <- which(sapply(1:dim(train)[2], function(z) is.numeric(train[,z])))
    # Elimino la clase en caso de que esté entre ellas
    variablesNumericas <- variablesNumericas[!variablesNumericas==y]
    
    modelo <- caret::train(train[,variablesNumericas], train.class,
                           method = "knn",
                           tuneLength = 10,
                           trControl = trainControl(method = "cv"))
    
    modelo.predict <- predict(modelo,test[,variablesNumericas])
    if(is.factor(modelo.predict)){
      x[instanciasPerdidas,y] <- as.character(modelo.predict)
    } else {
      x[instanciasPerdidas,y] <- modelo.predict
    }
    
    x
  }
  #' Segunda parte de la función:
  #' Le paso todas las varaibles perdidas a la función anterior
  #' para ello voy pasando el mismo data.frame, con los datos originales en cada iteración
  #' y en cada una de ellas voy sustituyendo en una copia del dataframe los perdidos con
  #' las imputaciones.
  
  y <- x
  variablesPerdidas <- which(sapply((1:dim(x)[2]), function(z) any(is.na(x[,z]) | x[,z]=="")))
  for(i in variablesPerdidas){
    print(i)
    n <- imputacionKnn(x,i)
    y[,i] <- n[,i]
  }
  y
}

# Aplico la función, que tiene en cuenta tanto numeros como caracteres
train <- imputacionKnnTotal(train)
test <- imputacionKnnTotal(test)

################
# "Numerifico" #
################

# x0
train[which(train[,1]=="VS"), 1] <- 1
train[which(train[,1]=="S"), 1] <- 2
train[which(train[,1]=="M"), 1] <- 3
train[which(train[,1]=="L"), 1] <- 4
train[which(train[,1]=="VL"), 1] <- 5
train[,1] <- as.integer(train[,1])


test[which(test[,1]=="VS"), 1] <- 1
test[which(test[,1]=="S"), 1] <- 2
test[which(test[,1]=="M"), 1] <- 3
test[which(test[,1]=="L"), 1] <- 4
test[which(test[,1]=="VL"), 1] <- 5
test[,1] <- as.integer(test[,1])

#x14
train[which(train[,15]=="true"), 15] <- 1
train[which(train[,15]=="false"), 15] <- 0
train[,15] <- as.integer(train[,15])

test[which(test[,15]=="true"), 15] <- 1
test[which(test[,15]=="false"), 15] <- 0
test[,15] <- as.integer(test[,15])

#x17
train[,18] <- as.integer(substr(train[,18], 4, 4))

test[,18] <- as.integer(substr(test[,18], 4, 4))

#x51
train[which(train[,51]=="yes"), 51] <- 1
train[which(train[,51]=="no"), 51] <- 0
train[,51] <- as.integer(train[,51])

test[which(test[,51]=="yes"), 51] <- 1
test[which(test[,51]=="no"), 51] <- 0
test[,51] <- as.integer(test[,51])

#x61
train[,61] <- as.integer(substr(train[,61], 4, 5))

test[,61] <- as.integer(substr(test[,61], 4, 5))

#x63
train[which(train[,63]=="active"), 63] <- 1
train[which(train[,63]=="inactive"), 63] <- 0
train[,63] <- as.integer(train[,63])

test[which(test[,63]=="active"), 63] <- 1
test[which(test[,63]=="inactive"), 63] <- 0
test[,63] <- as.integer(test[,63])

# Guardo una copia con la que trabajo para todos
write.csv2(train, "data/Numericos_ImpKNN_SinCorr-TRAIN.csv", row.names = FALSE)
write.csv2(test, "data/Numericos_ImpKNN_SinCorr-TEST.csv", row.names = FALSE)

##############
# CV 5-Folds #
##############

# Función de validación cruzada con 5-folds
crossvalidation5 <- function(train){
  # Cross-Validation
  # Se le pasa el train que se va a usar para construir el modelo y automaticamente
  # hace todo
  
  set.seed(2)
  
  # Mezclo las instacias
  datos<-train[sample(nrow(train)),]
  
  # Creo 5 folds
  folds <- cut(seq(1,nrow(train)),breaks=5,labels=FALSE)
  
  # A través de un sapply hago la CV para cada fold
  precision <- mean(sapply(1:5, function(x){
    
    indices <- which(folds==x,arr.ind=TRUE)
    test <- datos[indices, ]
    train <-datos[-indices, ]
    
    
    train.class <- train[,dim(train)[2]]
    train.class0 <- as.integer(I(train.class==0))
    train.class1 <- as.integer(I(train.class==1))
    train.class2 <- as.integer(I(train.class==2))
    train.class3 <- as.integer(I(train.class==3))
    
    # Quito a train la clase
    train <- train[,-dim(train)[2]]
    
    # Dataframes
    training0 <- cbind(train, y=train.class0)
    training1 <- cbind(train, y=train.class1)
    training2 <- cbind(train, y=train.class2)
    training3 <- cbind(train, y=train.class3)
    
    
    # Modelos y predicciones
    modelo0 <- glm(y~., data=training0, family = "binomial", maxit=500)
    modelo.predict0 <- predict(modelo0,newdata=test ,type="response")
    modelo1 <- glm(y~., data=training1, family = "binomial", maxit=500)
    modelo.predict1 <- predict(modelo1,newdata=test ,type="response")
    modelo2 <- glm(y~., data=training2, family = "binomial", maxit=500)
    modelo.predict2 <- predict(modelo2,newdata=test ,type="response")
    modelo3 <- glm(y~., data=training3, family = "binomial", maxit=500)
    modelo.predict3 <- predict(modelo3,newdata=test ,type="response")
    
    predicciones <- ifelse(modelo.predict0>modelo.predict1 & modelo.predict0>modelo.predict2 & modelo.predict0>modelo.predict3, 0,
                           ifelse(modelo.predict1>modelo.predict2 & modelo.predict1>modelo.predict3, 1,
                                  ifelse(modelo.predict2>modelo.predict3, 2, 3) ))
    
    return(mean(predicciones==test[,dim(test)[2]]))
    
  }))
  
  return(precision)
}

###
# 
##

prediccionTest <- function(train, test){
  # Se le pasa un train y test, entrena con el primero y predice sobre el segundo
  
  set.seed(2)
  
  train.class <- train[,dim(train)[2]]
  train.class0 <- as.integer(I(train.class==0))
  train.class1 <- as.integer(I(train.class==1))
  train.class2 <- as.integer(I(train.class==2))
  train.class3 <- as.integer(I(train.class==3))
    
  # Quito a train la clase
  train <- train[,-dim(train)[2]]
    
  # Dataframes
  training0 <- cbind(train, y=train.class0)
  training1 <- cbind(train, y=train.class1)
  training2 <- cbind(train, y=train.class2)
  training3 <- cbind(train, y=train.class3)
    
    
  # Modelos y predicciones
  modelo0 <- glm(y~., data=training0, family = "binomial", maxit=500)
  modelo.predict0 <- predict(modelo0,newdata=test ,type="response")
  modelo1 <- glm(y~., data=training1, family = "binomial", maxit=500)
  modelo.predict1 <- predict(modelo1,newdata=test ,type="response")
  modelo2 <- glm(y~., data=training2, family = "binomial", maxit=500)
  modelo.predict2 <- predict(modelo2,newdata=test ,type="response")
  modelo3 <- glm(y~., data=training3, family = "binomial", maxit=500)
  modelo.predict3 <- predict(modelo3,newdata=test ,type="response")
    
  predicciones <- ifelse(modelo.predict0>modelo.predict1 & modelo.predict0>modelo.predict2 & modelo.predict0>modelo.predict3, 0,
                           ifelse(modelo.predict1>modelo.predict2 & modelo.predict1>modelo.predict3, 1,
                                  ifelse(modelo.predict2>modelo.predict3, 2, 3) ))
  
  # Guardo aparte las predicciones para devolverlas al final
  resultados <- predicciones
  
  # Creo un tabla de predicciones para subirlo a Kaggle
  predicciones <- matrix(c(1:length(predicciones),predicciones), length(predicciones), 2,byrow = FALSE)
  colnames(predicciones) <- c("Id","Prediction")
  
  # Guardo
  write.table(predicciones, "sample.csv", col.names = TRUE, row.names = FALSE,
              quote=FALSE, sep = ",")
  
  return(resultados)
}

###########
# Modelos #
###########

###########
# 1. Base #
###########

crossvalidation5(train) # 0.6400181
prediccionTest(train, test)

# Gráfico para ver el acierto sobre train
prediccionesBase <- prediccionTest(train, train)

ggplot2::ggplot()+
  geom_col(aes(x=0:3, y=as.data.frame(table(train[,75]))[,2], fill="Original"), alpha=.8) + 
  geom_col(aes(x=0:3, y=as.data.frame(diag(table(train[,75], prediccionesBase)))[,1], fill="Predicciones"), alpha=.8) +
  scale_fill_discrete("Datos") +
  labs(x = "Clase", y = "Valores")

##########################
# 2. Selección variables #
##########################
# A través de Random Forest obtengo un vecto ordenado en función de la importancia
# de las variables que intervienen en él

library(randomForest)
rm <-randomForest::randomForest(train[,-75], as.factor(train[,75]))
importance <- caret::varImp(rm, scale=FALSE)
var <- order(importance[[1]])

crossvalidation5(train[,-var[1:25]]) # 0.6506509
prediccionTest(train[,-var[1:25]], test)

##########
# 3. IPF #
##########

trainIPF <- train

library(NoiseFiltersR)
# Factorizo la clase para aplicarle IPF
trainIPF[,75] <- factor(trainIPF[,75])
# Le quito las instancias con ruido
# indico que pare a la segunda que no encuentre y que lo haga mediante consenso
trainIPF <- IPF(y ~., trainIPF, consensus = TRUE, s=2)
# Me quedo unicamente con el data.frame limpio
trainIPF <- trainIPF$cleanData

crossvalidation5(trainIPF[,-var[1:25]]) # 0.6881916
prediccionTest(trainIPF[,-var[1:25]], test)

############
# 4. SMOTE #
############

trainSMOTE <- train

library(unbalanced)
# Aplico SMOTE sobre la clase 0 respecto al resto
instanciasSmote <- unbalanced::ubSMOTE(trainSMOTE[,-75], factor(as.integer(I(train[,75]==0))), perc.over = 200)
# Se unen todas las variables con la clase
instanciasSmote <-cbind(instanciasSmote[[1]], instanciasSmote[[2]])
# Me quedo con las instancias sintéticas para la clase minoritaria 0 que está marcadas como 1
instanciasSmote <- instanciasSmote[which(instanciasSmote[,75]==1),]
# Las convierto a 0
instanciasSmote[,75] <- 0
# Cambio el nombre de la clase a y
names(instanciasSmote)[75] <- "y"
# Añado los SMOTE a train
trainSMOTE <- rbind(trainSMOTE, instanciasSmote)

crossvalidation5(trainSMOTE[,-var[1:25]]) # 0.6523741
prediccionTest(trainSMOTE[,-var[1:25]], test)

##################
# 5. IPF + SMOTE #
##################

trainIPF_SMOTE <- train

# Factorizo la clase para aplicarle IPF
trainIPF_SMOTE[,75] <- factor(trainIPF_SMOTE[,75])
# Le quito las instancias con ruido
# indico que pare a la segunda que no encuentre y que lo haga mediante consenso
trainIPF_SMOTE <- IPF(y ~., trainIPF_SMOTE, consensus = TRUE, s=2)
# Me quedo unicamente con el data.frame limpio
trainIPF_SMOTE <- trainIPF_SMOTE$cleanData

# Aplico SMOTE sobre la clase 0 respecto al resto
instanciasSmote <- unbalanced::ubSMOTE(trainIPF_SMOTE[,-75], factor(as.integer(I(trainIPF_SMOTE[,75]==0))), perc.over = 200)
# Se unen todas las variables con la clase
instanciasSmote <-cbind(instanciasSmote[[1]], instanciasSmote[[2]])
# Me quedo con las instancias sintéticas para la clase minoritaria 0 que está marcadas como 1
instanciasSmote <- instanciasSmote[which(instanciasSmote[,75]==1),]
# Las convierto a 0
instanciasSmote[,75] <- 0
# Cambio el nombre de la clase a y
names(instanciasSmote)[75] <- "y"
# Añado los SMOTE a train
trainIPF_SMOTE <- rbind(trainIPF_SMOTE, instanciasSmote)

crossvalidation5(trainIPF_SMOTE[,-var[1:25]]) # 0.7006897
prediccionTest(trainIPF_SMOTE[,-var[1:25]], test)

##################
# 6. SMOTE + IPF #
##################

trainSMOTE_IPF <- train

# Aplico SMOTE sobre la clase 0 respecto al resto
instanciasSmote <- unbalanced::ubSMOTE(trainSMOTE_IPF[,-75], factor(as.integer(I(trainSMOTE_IPF[,75]==0))), perc.over = 200)
# Se unen todas las variables con la clase
instanciasSmote <-cbind(instanciasSmote[[1]], instanciasSmote[[2]])
# Me quedo con las instancias sintéticas para la clase minoritaria 0 que está marcadas como 1
instanciasSmote <- instanciasSmote[which(instanciasSmote[,75]==1),]
# Las convierto a 0
instanciasSmote[,75] <- 0
# Cambio el nombre de la clase a y
names(instanciasSmote)[75] <- "y"
# Añado los SMOTE a train
trainSMOTE_IPF <- rbind(trainSMOTE_IPF, instanciasSmote)

# Factorizo la clase para aplicarle IPF
trainSMOTE_IPF[,75] <- factor(trainSMOTE_IPF[,75])
# Le quito las instancias con ruido
# indico que pare a la segunda que no encuentre y que lo haga mediante consenso
trainSMOTE_IPF <- IPF(y ~., trainSMOTE_IPF, consensus = TRUE, s=2)
# Me quedo unicamente con el data.frame limpio
trainSMOTE_IPF <- trainSMOTE_IPF$cleanData

crossvalidation5(trainSMOTE_IPF[,-var[1:25]]) # 0.6690093
prediccionTest(trainSMOTE_IPF[,-var[1:25]], test)

##################
# 7. Tomek Links #
##################

trainTL <- train

# Busco las instancias Tomek Links
instanciasTL<- unbalanced::ubTomek(trainTL[,-75], (trainTL[,75]==0))
# Elimino esas instancias de train
trainTL <- trainTL[-instanciasTL$id.rm,]

crossvalidation5(trainTL[,-var[1:25]]) # 0.64294
prediccionTest(trainTL[,-var[1:25]], test)

##########################
# 8. Tomek Links + SMOTE #
##########################

trainTL_SMOTE <- train

# Busco las instancias Tomek Links
instanciasTL<- unbalanced::ubTomek(trainTL_SMOTE[,-75], (trainTL_SMOTE[,75]==0))
# Elimino esas instancias de train
trainTL_SMOTE <- trainTL_SMOTE[-instanciasTL$id.rm,]

# Aplico SMOTE sobre la clase 0 respecto al resto
instanciasSmote <- unbalanced::ubSMOTE(trainTL_SMOTE[,-75], factor(as.integer(I(trainTL_SMOTE[,75]==0))), perc.over = 200)
# Se unen todas las variables con la clase
instanciasSmote <-cbind(instanciasSmote[[1]], instanciasSmote[[2]])
# Me quedo con las instancias sintéticas para la clase minoritaria 0 que está marcadas como 1
instanciasSmote <- instanciasSmote[which(instanciasSmote[,75]==1),]
# Las convierto a 0
instanciasSmote[,75] <- 0
# Cambio el nombre de la clase a y
names(instanciasSmote)[75] <- "y"
# Añado los SMOTE a train
trainTL_SMOTE <- rbind(trainTL_SMOTE, instanciasSmote)

crossvalidation5(trainTL_SMOTE[,-var[1:25]]) # 0.6566631
prediccionTest(trainTL_SMOTE[,-var[1:25]], test)

##########################
# 9. SMOTE + Tomek Links #
##########################

trainSMOTE_TL <- train

# Aplico SMOTE sobre la clase 0 respecto al resto
instanciasSmote <- unbalanced::ubSMOTE(trainSMOTE_TL[,-75], factor(as.integer(I(trainSMOTE_TL[,75]==0))), perc.over = 200)
# Se unen todas las variables con la clase
instanciasSmote <-cbind(instanciasSmote[[1]], instanciasSmote[[2]])
# Me quedo con las instancias sintéticas para la clase minoritaria 0 que está marcadas como 1
instanciasSmote <- instanciasSmote[which(instanciasSmote[,75]==1),]
# Las convierto a 0
instanciasSmote[,75] <- 0
# Cambio el nombre de la clase a y
names(instanciasSmote)[75] <- "y"
# Añado los SMOTE a train
trainSMOTE_TL <- rbind(trainSMOTE_TL, instanciasSmote)

# Busco las instancias Tomek Links
instanciasTL<- unbalanced::ubTomek(trainSMOTE_TL[,-75], (trainSMOTE_TL[,75]==0))
# Elimino esas instancias de train
trainSMOTE_TL <- trainSMOTE_TL[-instanciasTL$id.rm,]

crossvalidation5(trainSMOTE_TL[,-var[1:25]]) # 0.6534006
prediccionTest(trainSMOTE_TL[,-var[1:25]], test)

###########
# 10. ROS #
###########

trainROS <- train

library(ROSE)
# Factorizo la clase
trainROS[,75] <- factor(trainROS[,75])
# Modifico los levels de la clase para aplicar ROS
levels(trainROS[,75]) <- c(0,1,1,1)
instanciasROS <- ROSE::ovun.sample(y ~., trainROS, method = "over")
# Añado a train las instancias ROS de la clase 0
trainROS <- rbind(train, instanciasROS$data[which(instanciasROS$data[,75]==0),])

crossvalidation5(trainROS[,-var[1:25]]) # 0.7229307
prediccionTest(trainROS[,-var[1:25]], test)


#################
# 11. ROS + IPF #
#################

trainROS_IPF <- train

# Factorizo la clase
trainROS_IPF[,75] <- factor(trainROS_IPF[,75])
# Modifico los levels de la clase para aplicar ROS
levels(trainROS_IPF[,75]) <- c(0,1,1,1)
instanciasROS <- ROSE::ovun.sample(y ~., trainROS_IPF, method = "over")
# Añado a train las instancias ROS de la clase 0
trainROS_IPF <- rbind(train, instanciasROS$data[which(instanciasROS$data[,75]==0),])

# Factorizo la clase para aplicarle IPF
trainROS_IPF[,75] <- factor(trainROS_IPF[,75])
# Le quito las instancias con ruido
# indico que pare a la segunda que no encuentre y que lo haga mediante consenso
trainROS_IPF <- IPF(y ~., trainROS_IPF, consensus = TRUE, s=2)
# Me quedo unicamente con el data.frame limpio
trainROS_IPF <- trainROS_IPF$cleanData

crossvalidation5(trainROS_IPF[,-var[1:25]]) # 0.7340467
prediccionTest(trainROS_IPF[,-var[1:25]], test)

#############
# 12. ROS 2 #
#############

train_ROS2 <- train

# Hago ROS de la clase 0 con cada una del resto de manera individual y prestando
# más atención a la clase 1
train_ROS2_1 <- train[which(train_ROS2[,75]==0|train_ROS2[,75]==1),]
train_ROS2_1 <- ROSE::ovun.sample(y ~., train_ROS2_1, method = "over", p=0.5)
train_ROS2_1 <- train_ROS2_1$data
levels(train_ROS2_1) <- c(0,1)

train_ROS2_2 <- train[which(train_ROS2[,75]==0|train_ROS2[,75]==2),]
train_ROS2_2 <- ROSE::ovun.sample(y ~., train_ROS2_2, method = "over", p=0.3)
train_ROS2_2 <- train_ROS2_2$data
levels(train_ROS2_2) <- c(0,2)

train_ROS2_3 <- train[which(train_ROS2[,75]==0|train_ROS2[,75]==1),]
train_ROS2_3 <- ROSE::ovun.sample(y ~., train_ROS2_3, method = "over", p=0.3)
train_ROS2_3 <- train_ROS2_3$data
levels(train_ROS2_3) <- c(0,3)

train_ROS2 <- rbind(train, train_ROS2_1, train_ROS2_2, train_ROS2_3)

crossvalidation5(train_ROS2[,-var[1:25]]) # 0.6478257
prediccionTest(train_ROS2[,-var[1:25]], test)

###################
# 13. ROS 2 + IPF #
###################

train_ROS2_IPF <- train

# Hago ROS de la clase 0 con cada una del resto de manera individual y prestando
# más atención a la clase 1
train_ROS2_1 <- train[which(train_ROS2_IPF[,75]==0|train_ROS2_IPF[,75]==1),]
train_ROS2_1 <- ROSE::ovun.sample(y ~., train_ROS2_1, method = "over", p=0.5)
train_ROS2_1 <- train_ROS2_1$data
levels(train_ROS2_1) <- c(0,1)

train_ROS2_2 <- train[which(train_ROS2_IPF[,75]==0|train_ROS2_IPF[,75]==2),]
train_ROS2_2 <- ROSE::ovun.sample(y ~., train_ROS2_2, method = "over", p=0.3)
train_ROS2_2 <- train_ROS2_2$data
levels(train_ROS2_2) <- c(0,2)

train_ROS2_3 <- train[which(train_ROS2_IPF[,75]==0|train_ROS2_IPF[,75]==1),]
train_ROS2_3 <- ROSE::ovun.sample(y ~., train_ROS2_3, method = "over", p=0.3)
train_ROS2_3 <- train_ROS2_3$data
levels(train_ROS2_3) <- c(0,3)

train_ROS2_IPF <- rbind(train, train_ROS2_1, train_ROS2_2, train_ROS2_3)

# Factorizo la clase para aplicarle IPF
train_ROS2_IPF[,75] <- factor(train_ROS2_IPF[,75])
# Le quito las instancias con ruido
# indico que pare a la segunda que no encuentre y que lo haga mediante consenso
train_ROS2_IPF <- IPF(y ~., train_ROS2_IPF, consensus = TRUE, s=2)
# Me quedo unicamente con el data.frame limpio
train_ROS2_IPF <- train_ROS2_IPF$cleanData

crossvalidation5(train_ROS2_IPF[,-var[1:25]]) # 0.6639876
prediccionTest(train_ROS2_IPF[,-var[1:25]], test)

###########
# 14. ENN #
###########

trainENN <- train

instanciasENN <- unbalanced::ubENN(trainENN[,-75], (trainENN[,75]==0))
trainENN <- trainENN[-instanciasENN$id.rm,]

crossvalidation5(trainENN[,-var[1:25]]) # 0.7550752
prediccionTest(trainENN[,-var[1:25]], test)

##################
# 15. Duplicar 0 #
##################

trainROS_Propio <- train

trainROS_Propio <- rbind(trainROS_Propio, train[which(trainROS_Propio[,75]==0),])

crossvalidation5(trainROS_Propio[,-var[1:25]]) # 0.643618
prediccionTest(trainROS_Propio[,-var[1:25]], test)

#############################
# 16. Duplicar 0 + Random 1 #
#############################

trainROS_Propio <- train

trainROS_Propio <- rbind(trainROS_Propio, train[which(trainROS_Propio[,75]==0),])
trainROS_Propio <- rbind(trainROS_Propio, train[sample(which(trainROS_Propio[,75]==1), length(which(trainROS_Propio[,75]==1))/2.5),]) 

crossvalidation5(trainROS_Propio[,-var[1:25]]) # 0.6458915
prediccionTest(trainROS_Propio[,-var[1:25]], test)

# Gráfico para ver el acierto sobre train (Mejora el acierto de la clase 0 pero se reducen en la dos)
prediccionesFinal <- prediccionTest(trainROS_Propio[,-var[1:25]], train)

ggplot2::ggplot()+
  geom_col(aes(x=0:3, y=as.data.frame(table(train[,75]))[,2], fill="Original"), alpha=.8) + 
  geom_col(aes(x=0:3, y=as.data.frame(diag(table(train[,75], prediccionesFinal)))[,1], fill="Predicciones"), alpha=.8) +
  scale_fill_discrete("Datos") +
  labs(x = "Clase", y = "Valores")

