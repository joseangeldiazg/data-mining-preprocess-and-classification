#Pasos a seguir para la confeccion del modelo
# 1. Analisis preliminar
# 1a. Grafico de pares
# 1b. Numerico: Coeficientes de correlacion
# 2. Construccion del modelo
# 2a. Definicion del modelo
# 3. Estimacion de la bondad del modelo construido
# 3a. Estimacion del sigma
# 3b. ANOVA
# 3c. Coeficiente de determinacion
# 3d. Capacidad de prediccion
# 3e. Normalidad
# 3f. Homocedasticidad
# 3g. Incorrelacion

library(ISLR)



# Declaracion de funciones

# Calculo de la medida del error standar medio (MSE) y 
# el porcentaje de error sobre el rango
# La variable "y" esta en datos[,1]
MSE <- function(datos,regresion){
  yprime <- predict(regresion, datos)
  b <-sum((datos[,1]-yprime)^2)/length(yprime) ##MSE
  b <- as.vector(b)
  b[2]<- (b[1]/(range(datos[,1])[2]-range(datos[,1])[1]^2))*100
  return(b)
}



# Funciones para realizar la validacion cruzada
library(bootstrap)

theta.fit <- function(v,w,ff=model){
  a <- 0
  if (ff$call[1]=="lm()"){
    a <-lm(as.formula(ff$call$formula), data=as.data.frame(cbind(v,y=w)))
  }
  else{ if  (ff$call[1]=="gam()"){
    a <-gam(as.formula(ff$call$formula), data=as.data.frame(cbind(v,y=w)))
  }
  }  
  a
}

theta.predict <- function(fit, x) {
  if (is.null(dim(x))) {
    x <- t(x)
  }
  predict(fit, newdata=as.data.frame(x))
}


ValidacionCruzada <- function(datos, particiones, model){
  a <- crossval(datos[,-1], datos[,1], theta.fit, theta.predict, ngroup=particiones, ff=model)
  b <- (sum(abs(a$cv.fit - datos$y)^2)/length(datos$y)) ##MSE
  
  #Porcentaje de error sobre el rango
  c <- sum(abs(a$cv.fit - datos$y))/length(datos$y)
  b[2] <- (c*100)/abs(range(datos$y)[2]-range(datos$y)[1])
  b <- as.vector(b)
  names(b) <- cbind("MSE","%onRange")
  return (b)
}





# Visualizacion del ajuste
visualiza_datos <- function(datos, model){
  datos_ord <-datos[sort(datos[,1], index.return=TRUE)$ix,]
  plot(1:dim(datos_ord)[1],datos_ord$y,xlab="ejemplo", ylab="y",type="p")
  pred <- predict(model, newdata = datos_ord)
  points(1:dim(datos_ord)[1],pred, col="red")
  
  plot(1:dim(datos_ord)[1],datos_ord$y,xlab="ejemplo", ylab="y",type="l")
  segments(1:dim(datos_ord)[1], datos_ord$y, 1:dim(datos_ord)[1], pred,col="red", lty = 1)
  
}


AnalisisGrafico <- function (datos, model){
  
  par(mfrow=c(2,2))
  
  # histograma Normalidad
  e <-residuals(model)
  d <- e/summary(model)$sigma
  
  hist (d, probability = T, xlab = "Errores estandar", main = "", xlim = c(-3,3))
  
  d.seq <- seq(-3,3,length = 50)
  
  lines(d.seq, dnorm(d.seq, mean(d), sd(d)), col="red")
  
  # Incorrelacion
  
  n <- length(d)
  plot(d[1:n-1],d[2:n],xlab = "Error i", ylab = "Error i-1")
  lines(lowess(d[1:n-1],d[2:n]),col="red")
  
  # Representacion del resultado
  
  visualiza_datos(datos,model)
}



# Procesamiento de propiedades del modelo
Analisis <- function (datos, model){
  resumen_model = summary(model)
  # Error Estandar Residual
  a <-100*(sd(datos$y)/(mean(datos$y)))
  at <- ifelse(a<10,"Si","No")
  a <- format(a,digits = 3)
  
  
  # ANOVA
  b <- 0
  bt <- "--"
  if (model$call[1]!="gam()"){
    b <- pf(resumen_model$fstatistic[1L], 
            resumen_model$fstatistic[2L], 
            resumen_model$fstatistic[3L], 
            lower.tail = FALSE)
    
    bt <- ifelse(b<0.05,"Si", "No")
    names(bt) <- c()
    b <- format(b,digits = 3)
  }
  
  # Coeficiente de determinacion R2
  Ypred = predict(model, data = datos)
  
  VT = sum( (Yreal-mean(Yreal))*(Yreal-mean(Yreal)) )
  VE = sum( (Ypred-mean(Yreal))*(Ypred-mean(Yreal)) )
  VR = sum(  (Yreal-Ypred) * (Yreal-mean(Yreal)))
  
  R2 = VE / VT
  n = length(Yreal)
  p = length((model))
  R2.corregido = 1 - (1-R2)*(n-1)/(n-p)
  c <- R2
  ct <- ifelse(c>0.8,"Si", "No")
  
  d <- format(d,digits = 3)
  c <- format(c,digits = 3)
  
  
  # 1. Test de Normalidad ################ COMPLETAR ###############################
  # En 'et' se asigna un "Si" si satisface el test o "No" en otro caso
  
  # 2. Homocedasticidad
  library(lmtest)
  f <-bptest(model)$p.value
  ft <- ifelse(f>=0.05,"Si", "No")
  names(ft)<-c()
  
  # 3. Incorrelacion ######################## COMPLETAR ##############################
  # En 'gt' se asigna un "Si" si satisface el test o "No" en otro caso
  library(lmtest)
  
  
  # 4. MSE
  h <- MSE(datos,model)
  h[1] <- format(h[1],digits = 3)
  #h[2] <- format(h[2],digits = 3)
  
  # Validacion cruzada
  library(bootstrap)
  i <- ValidacionCruzada(datos, 10, model) 
  i[1] <- format(i[1],digits = 3)
  i[2] <- format(i[2],digits = 3)
  
  data.frame(EER = at, ANOVA = bt, R2 = ct, Norm = et, Homoced = ft, Incorr = gt, MSE = h[1], CV = i[1], PError = i[2])
}




########################################### EJERCICIOS ######################################################





# Resolucion del ejercicio 1
datos <-data.frame( y=iris$Sepal.Width,
                   x1=iris$Sepal.Length,
                   x2=iris$Petal.Length,
                   x3=iris$Petal.Width)

model.ejercicio1 <- lm(y~., data = datos)
model.ejercicio1.Resultado <- Analisis(datos,model.ejercicio1)
model.ejercicio1.Resultado






# Resolucion del ejercicio 2
datos <-data.frame( y=iris$Petal.Length,
                    x1=iris$Sepal.Length,
                    x2=iris$Sepal.Width,
                    x3=iris$Petal.Width)


########## Completar desde aqui



########## Hasta aqui



# Comparacion entre los modelos del ejercicio 1 y el ejercicio2
df <- data.frame(rbind(ejer1=model.ejercicio1.Resultado,
                       ejer2=model.ejercicio2.Resultado),
                 stringsAsFactors = FALSE)
df





#Resolucion del ejercicio 3
datos <-data.frame(  y=trees$Girth,
                    x1=trees$Height,
                    x2=trees$Volume)



########## Completar desde aqui



########## Hasta aqui

AnalisisGrafico (datos, model.ejercicio3)

df <- data.frame(rbind(ejer1=model.ejercicio1.Resultado,
                       ejer2=model.ejercicio2.Resultado,
                       ejer3=model.ejercicio3.Resultado),
                 stringsAsFactors = FALSE)
df



#Resolucion del ejercicio 4
datos <-data.frame( y=iris$Sepal.Width,
                    x1=iris$Sepal.Length,
                    x2=iris$Petal.Length,
                    x3=iris$Petal.Width)


########## Completar desde aqui



########## Hasta aqui

df <- data.frame(rbind(model1=model.ejercicio41.Resultado,
                       model2=model.ejercicio42.Resultado,
                       model3=model.ejercicio43.Resultado,
                       model4=model.ejercicio44.Resultado,
                       model5=model.ejercicio45.Resultado),
                 stringsAsFactors = FALSE)
df







#Resolucion del ejercicio 5
datos <-data.frame( y=iris$Sepal.Width,
                    x1=iris$Sepal.Length,
                    x2=iris$Petal.Length,
                    x3=iris$Petal.Width)
library(splines)

########## Completar desde aqui



########## Hasta aqui


df <- data.frame(rbind(model1=model.ejercicio51.Resultado,
                       model2=model.ejercicio52.Resultado,
                       model3=model.ejercicio53.Resultado,
                       model4=model.ejercicio54.Resultado,
                       model5=model.ejercicio55.Resultado),
                 stringsAsFactors = FALSE)
df













#Resolucion del ejercicio 6
datos <-data.frame( y=iris$Sepal.Width,
                    x1=iris$Sepal.Length,
                    x2=iris$Petal.Length,
                    x3=iris$Petal.Width)
library(splines)
library(gam)

########## Completar desde aqui



########## Hasta aqui


df <- data.frame(rbind(model1=model.ejercicio61.Resultado,
                       model2=model.ejercicio62.Resultado,
                       model3=model.ejercicio63.Resultado,
                       model4=model.ejercicio64.Resultado),
                 stringsAsFactors = FALSE)
df



#Resolucion del Ejercicio 7
datos <-data.frame(  y=trees$Volume,
                     x1=trees$Girth,
                     x2=trees$Height)

# Apartado a

########## Completar desde aqui



########## Hasta aqui


df <- data.frame(rbind(model1=model.ejercicio61.Resultado,
                       model2=model.ejercicio62.Resultado,
                       model3=model.ejercicio63.Resultado,
                       model4=model.ejercicio64.Resultado),
                 stringsAsFactors = FALSE)
df

df <- data.frame(rbind(model1 = model.ejercicio7a1.Resultado,
               model2 =model.ejercicio7a2.Resultado,
               model3 =model.ejercicio7a3.Resultado,
               model4 =model.ejercicio7a4.Resultado))

df
# Apartado b


########## Completar desde aqui



########## Hasta aqui


df <- data.frame(rbind(model1 = model.ejercicio7b1.Resultado,
                       model2 = model.ejercicio7b2.Resultado,
                       model3 = model.ejercicio7b3.Resultado,
                       model4 = model.ejercicio7b4.Resultado))

df


# Apartado c


########## Completar desde aqui



########## Hasta aqui


df <- data.frame(rbind(poly1=model.ejercicio7a1.Resultado,
                       poly2=model.ejercicio7a2.Resultado,
                       poly3=model.ejercicio7a3.Resultado,
                       poly4=model.ejercicio7a4.Resultado,
                       bs1=model.ejercicio7b1.Resultado,
                       bs2=model.ejercicio7b2.Resultado,
                       bs3=model.ejercicio7b3.Resultado,
                       bs4=model.ejercicio7b4.Resultado,
                       gam1=model.ejercicio7c1.Resultado,
                       gam2=model.ejercicio7c2.Resultado,
                       gam3=model.ejercicio7c3.Resultado,
                       gam4=model.ejercicio7c4.Resultado
                       ), stringsAsFactors = FALSE)
df
