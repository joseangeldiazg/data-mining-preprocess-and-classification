# Cross-Validation propio para KNN

crossvalidation5 <- function(train){
    set.seed(2)
  
  datos<-train[sample(nrow(train)),]
  
  # Creo 5 folds
  folds <- cut(seq(1,nrow(train)),breaks=5,labels=FALSE)
  
  # CV para cada fold
  precision <- mean(sapply(1:5, function(x){
    
    indices <- which(folds==x,arr.ind=TRUE)
    test <- datos[indices, ]
    train <-datos[-indices, ]
    
    # A partir de aqui es especifico de mi modelo
    modelo <- caret::train(train[,-dim(train)[2]], train$y,
                    method = "knn",
                    preProcess = c("center","scale"),
                    tuneGrid = data.frame(.k=1))
    
    
    modelo.predict0 <- caret::predict.train(modelo, test, type="raw")
    predicciones <- as.integer(as.character(modelo.predict0))
    
    return(mean(predicciones==test[,dim(test)[2]]))
    
  }))
  
  return(precision)

}
