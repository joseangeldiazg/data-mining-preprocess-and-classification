setwd("/Users/wences/Desktop/Máster/Minería de datos Preprocesamiento y clasificación/Final/MdDPyC/glm")
# Carga de datos
Train <- read.csv2("data/Numericos_ImpKNN_SinCorr-TRAIN.csv", stringsAsFactors = FALSE)
Test <- read.csv2("data/Numericos_ImpKNN_SinCorr-TEST.csv", stringsAsFactors = FALSE)

# Las copio a variables nuevas para trabajar sobre ellas
train <- Train
test <- Test

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
    
    # A partir de aqui es especifico de mi modelo
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

crossvalidation5(train)

