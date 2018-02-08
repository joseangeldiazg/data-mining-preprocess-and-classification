library(Hmisc)
library(corrplot)
library(NoiseFiltersR)
library(rpart)
library(caret)
#Leemos los datos. 

train <- read.csv("data/train.csv")
test <- read.csv("data/test.csv")

#Comprobamos de que tipo  son los datos

str(train)
describe(train)

#X0 es categorica, x14 es categórica, x17 es categorica y encima tiene perdidos que se toman como vacios, x51 categorica 

#Tenemos muchas variables algunas con muchos valores perdidos, vamos a crear gráficos de correlacion para ver si podemos quitarnos del medio alguna

#Vamos a quedarnos solo con las numéricas

numeric<-sapply(train, is.numeric)

trainNumeric<-train[,numeric]

#Vamos a quedarnos solo con aquellas que no tengan valores perdidos en una primera aproximación

trainNumeric<-trainNumeric[complete.cases(trainNumeric),]

#Obtenemos las correlacioenes

M <- cor(trainNumeric[,1:20])
M2 <- cor(trainNumeric[,21:40])
M3 <- cor(trainNumeric[,41:60])
M4 <- cor(trainNumeric[,61:69])

corrplot(M4, method = "circle")

#No hay mucha correlación entre ninguna variable por lo que vamos a pasar a un filtrado de ruido básico y a clasificar usando todas las variables

trainComplete<-train[complete.cases(train),]

trainComplete$y<-as.factor(trainComplete$y)

#Aplicamos el filtro ipf

out <- IPF(y~., data = trainComplete, s = 2)

trainClean<-out$cleanData


#Clasificamos

control <- trainControl(method = "cv", number = 5, returnResamp = "all", search = "random")

trainModel <- train(y ~.,  data = trainClean, 
                            method = "rpart", 
                            trControl = control,
                            preProc = c("center", "scale"))


testing <- predict(trainModel, )

test[complete.cases(test),]

length(testing)

submision<-as.data.frame(cbind(1:683, Prediction,as.numeric(testing)))
