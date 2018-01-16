Acierto <- function(y1,y2){
  return (sum (sapply(1:length(y1), function(x){
    if (is.na(y2[x])){
      0
    } 
    else if (as.numeric(y1[x])==as.numeric(y2[x])){
      1
    }
    else{
      0
    }
  }))/length(y1))
}


library(ISLR)
library(splines)
setwd("/home/usuario/tmp/master2016_17/data")
bd <- read.csv("CoordenadasMunicipios.csv", header = T, sep = ",")
bd[,1] = as.numeric(bd[,1])

datos = as.data.frame(cbind(y= bd[,1], x1 = bd$longitud, x2 = bd$latitud))


set.seed(9)
muestra = sample(1:nrow(datos), 100)
train = as.data.frame(datos[-muestra, ])
test = as.data.frame(datos[muestra, ])

# modelo regresion logistica
d1 = as.data.frame(cbind(y= as.numeric(I(train$y==1)), x1 =train$x1, x2 =train$x2))
d2 = as.data.frame(cbind(y= as.numeric(I(train$y==2)), x1 =train$x1, x2 =train$x2))
d3 = as.data.frame(cbind(y= as.numeric(I(train$y==3)), x1 =train$x1, x2 =train$x2))
d4 = as.data.frame(cbind(y= as.numeric(I(train$y==4)), x1 =train$x1, x2 =train$x2))
d5 = as.data.frame(cbind(y= as.numeric(I(train$y==5)), x1 =train$x1, x2 =train$x2))
d6 = as.data.frame(cbind(y= as.numeric(I(train$y==6)), x1 =train$x1, x2 =train$x2))
d7 = as.data.frame(cbind(y= as.numeric(I(train$y==7)), x1 =train$x1, x2 =train$x2))
d8 = as.data.frame(cbind(y= as.numeric(I(train$y==8)), x1 =train$x1, x2 =train$x2))

mRL1 <- glm(y ~ ns(x1,4) + ns(x2,4) , data = d1)
mRL2 <- glm(y ~ ns(x1,4) + ns(x2,4) , data = d2)
mRL3 <- glm(y ~ ns(x1,4) + ns(x2,4) , data = d3)
mRL4 <- glm(y ~ ns(x1,4) + ns(x2,4) , data = d4)
mRL5 <- glm(y ~ ns(x1,4) + ns(x2,4) , data = d5)
mRL6 <- glm(y ~ ns(x1,4) + ns(x2,4) , data = d6)
mRL7 <- glm(y ~ ns(x1,4) + ns(x2,4) , data = d7)
mRL8 <- glm(y ~ ns(x1,4) + ns(x2,4) , data = d8)

SmRL <- cbind (predict(mRL1, newdata = train, type="response"), 
               predict(mRL2, newdata = train, type="response"),
               predict(mRL3, newdata = train, type="response"),
               predict(mRL4, newdata = train, type="response"),
               predict(mRL5, newdata = train, type="response"),
               predict(mRL6, newdata = train, type="response"),
               predict(mRL7, newdata = train, type="response"),
               predict(mRL8, newdata = train, type="response"))


salida = sapply(1:nrow(SmRL), function(x) {which.max(SmRL[x,])})

Acierto(train$y,salida)

SmRLTest <- cbind (predict(mRL1, newdata = test, type="response"), 
               predict(mRL2, newdata = test, type="response"),
               predict(mRL3, newdata = test, type="response"),
               predict(mRL4, newdata = test, type="response"),
               predict(mRL5, newdata = test, type="response"),
               predict(mRL6, newdata = test, type="response"),
               predict(mRL7, newdata = test, type="response"),
               predict(mRL8, newdata = test, type="response"))


salidaTest = sapply(1:nrow(SmRLTest), function(x) {which.max(SmRLTest[x,])})

Acierto(test$y,salidaTest)

# Pintado del resultado

plot(train$x1,train$x2,col=train$y)
points(train$x1, train$x2, col=train$y, pch = "x")


# Pintado de espacios de decision
grid.lines = 200
x.pred <- seq(min(datos$x2), max(datos$x2), length.out = grid.lines)
z.pred <- seq(min(datos$x1), max(datos$x1), length.out = grid.lines)
xz <- expand.grid( x2 = x.pred, x1 = z.pred)
SmRLTotal <- cbind (predict(mRL1, newdata = xz, type="response"), 
               predict(mRL2, newdata = xz, type="response"),
               predict(mRL3, newdata = xz, type="response"),
               predict(mRL4, newdata = xz, type="response"),
               predict(mRL5, newdata = xz, type="response"),
               predict(mRL6, newdata = xz, type="response"),
               predict(mRL7, newdata = xz, type="response"),
               predict(mRL8, newdata = xz, type="response"))
salidaTotal1 = sapply(1:nrow(SmRLTotal), function(x) {which.max(SmRLTotal[x,])})


plot(xz[,2], xz[,1], col= salidaTotal1, pch='*')
points(datos$x1, datos$x2, col=datos$y, pch = "x")


# modelo arbol (Tipo CART)

library(tree)

train[,1] = as.factor(train[,1])
mT <- tree(y ~ x1 + x2 , data = train)

SmT <- predict(mT, newdata = train, type = "class")
SmTTest <- predict(mT, newdata = test, type ="class")

Acierto(train$y,SmT)
Acierto(test$y,SmTTest)

plot(train$x1,train$x2,col=train$y,pch=0, 
     xlab = "Longitud", ylab = "Latitud", title("Provincias"))
points(train$x1,train$x2,col=SmT,pch="x")

# Pintado de espacios de decision
x.pred <- seq(min(datos$x2), max(datos$x2), length.out = grid.lines)
z.pred <- seq(min(datos$x1), max(datos$x1), length.out = grid.lines)
xz <- expand.grid( x2 = x.pred, x1 = z.pred)

salidaTotal2 <- predict(mT, newdata = xz, type = "class")


points(xz[,2], xz[,1], col = salidaTotal2, pch="*")


# Random Forest
library (randomForest)

mRF = randomForest(y~x1+x2, data=train)

SmRF = predict(mRF, data= train)
SmRFTest = predict(mRF, data= test)

Acierto(train$y, SmRF)
Acierto(train$y, SmRFTest)

plot(train$x1,train$x2,col=train$y,pch=0, 
     xlab = "Longitud", ylab = "Latitud", title("Provincias"))
points(train$x1,train$x2,col=SmRF,pch="x")

# Pintado de espacios de decision
x.pred <- seq(min(datos$x2), max(datos$x2), length.out = grid.lines)
z.pred <- seq(min(datos$x1), max(datos$x1), length.out = grid.lines)
xz <- expand.grid( x2 = x.pred, x1 = z.pred)

salidaTotal3 <- predict(mRF, newdata = xz, type = "class")


points(xz[,2], xz[,1], col = salidaTotal3, pch="*")



# Boosting
library (gbm)

mB =gbm(y~x1+x2, data=train, 
                distribution="multinomial",n.trees =500,
                interaction.depth =4)


SmB = predict (mB ,newdata = train, n.trees =500)
SmB = as.data.frame(SmB)
SmBT = sapply(1:nrow(SmB), function(x) {which.max(SmB[x,])})

SmBTes = predict (mB ,newdata = test, n.trees =500)
SmBTes = as.data.frame(SmBTes)
SmBTest = sapply(1:nrow(SmBTes), function(x) {which.max(SmBTes[x,])})

Acierto(train$y,SmBT)
Acierto(test$y,SmBTest)

plot(train$x1,train$x2,col=train$y,pch=0, 
     xlab = "Longitud", ylab = "Latitud", title("Provincias"))
points(train$x1,train$x2,col=SmBT,pch="x")


plot(test$x1,test$x2,col=test$y,pch=0, 
     xlab = "Longitud", ylab = "Latitud", title("Provincias"))
points(test$x1,test$x2,col=SmBTest,pch="x")

# Pintado de espacios de decision
x.pred <- seq(min(datos$x2), max(datos$x2), length.out = grid.lines)
z.pred <- seq(min(datos$x1), max(datos$x1), length.out = grid.lines)
xz <- expand.grid( x2 = x.pred, x1 = z.pred)

d <- predict(mB, newdata = xz, n.trees = 500)
d = as.data.frame(d)
salidaTotal4 = sapply(1:nrow(d), function(x) {which.max(d[x,])})


points(xz[,2], xz[,1], col = salidaTotal4, pch="*")

# Arboles (C4.5)
library(RWeka)

mC4 = J48(y~x1+x2, data=train)

SmC4 = predict(mC4, newdata=train)
SmC4Test = predict(mC4, newdata=test)


Acierto(train$y,SmC4)
Acierto(test$y,SmC4Test)

plot(train$x1,train$x2,col=train$y,pch=0, 
     xlab = "Longitud", ylab = "Latitud", title("Provincias"))
points(train$x1,train$x2,col=SmC4,pch="x")


plot(test$x1,test$x2,col=test$y,pch=0, 
     xlab = "Longitud", ylab = "Latitud", title("Provincias"))
points(test$x1,test$x2,col=SmC4Test,pch="x")

# Pintado de espacios de decision
x.pred <- seq(min(datos$x2), max(datos$x2), length.out = grid.lines)
z.pred <- seq(min(datos$x1), max(datos$x1), length.out = grid.lines)
xz <- expand.grid( x2 = x.pred, x1 = z.pred)

salidaTotal5 <- predict(mC4, newdata = xz)
points(xz[,2], xz[,1], col = salidaTotal5, pch="*")



# Ripper

mRip = JRip(y~x1+x2, data=train)

SmRip = predict(mRip, newdata=train)
SmRipTest = predict(mRip, newdata=test)


Acierto(train$y,SmRip)
Acierto(test$y,SmRipTest)

plot(train$x1,train$x2,col=train$y,pch=0, 
     xlab = "Longitud", ylab = "Latitud", title("Provincias"))
points(train$x1,train$x2,col=SmRip,pch="x")


plot(test$x1,test$x2,col=test$y,pch=0, 
     xlab = "Longitud", ylab = "Latitud", title("Provincias"))
points(test$x1,test$x2,col=SmRipTest,pch="x")

# Pintado de espacios de decision
x.pred <- seq(min(datos$x2), max(datos$x2), length.out = grid.lines)
z.pred <- seq(min(datos$x1), max(datos$x1), length.out = grid.lines)
xz <- expand.grid( x2 = x.pred, x1 = z.pred)

salidaTotal6 <- predict(mRip, newdata = xz)
points(xz[,2], xz[,1], col = salidaTotal6, pch="*")



################################# Agregando todas las salida

par(mfrow=c(3,2))
plot(datos$x1,datos$x2,col=0,pch=".", 
     xlab = "Longitud", ylab = "Latitud", title("glm"))
points(xz[,2], xz[,1], col = salidaTotal1, pch="*")

plot(datos$x1,datos$x2,col=0,pch=".", 
     xlab = "Longitud", ylab = "Latitud", title("CART"))
points(xz[,2], xz[,1], col = salidaTotal2, pch="*")

plot(datos$x1,datos$x2,col=0,pch=".", 
     xlab = "Longitud", ylab = "Latitud", title("randomForest"))
points(xz[,2], xz[,1], col = salidaTotal3, pch="*")

plot(datos$x1,datos$x2,col=0,pch=".", 
     xlab = "Longitud", ylab = "Latitud", title("Boosting"))
points(xz[,2], xz[,1], col = salidaTotal4, pch="*")

plot(datos$x1,datos$x2,col=0,pch=".", 
     xlab = "Longitud", ylab = "Latitud", title("C4.5"))
points(xz[,2], xz[,1], col = salidaTotal5, pch="*")

plot(datos$x1,datos$x2,col=0,pch=".", 
     xlab = "Longitud", ylab = "Latitud", title("Ripper"))
points(xz[,2], xz[,1], col = salidaTotal6, pch="*")

par(mfrow=c(1,1))

Resul = rbind(ifelse(salidaTotal1==salidaTotal2 & salidaTotal2==salidaTotal3 & salidaTotal3==salidaTotal4 
               & salidaTotal4 == salidaTotal5 & salidaTotal5== salidaTotal6, salidaTotal1, 0))


# Grado de coincidencia

plot(xz[,2], xz[,1], col = Resul, pch="*", 
     xlab = "Longitud", ylab = "Latitud", title("Com??n a todos los clasificadores"))

points(datos$x1,datos$x2,col=datos$y,pch="x")


sum(sapply(1:length(Resul), function(x){
  ifelse(Resul[x]==0,0,1)
}))/length(Resul)
