#Imputación Valores Perdidos

#joseangeldiazg

#En este script, vamos a generar distintos datasets con distintas variacioenes de imputacion de perdidos
#para ser usadas estas en nuestros experimentos


#Librerias

library(Hmisc)
library(corrplot)
library(NoiseFiltersR)
library(rpart)
library(caret)
library(mice)
library(DMwR)
library(dummies)
library(unbalanced)

# Leemos los datos

TrainNormal <- read.csv2("data/my_dataset_train.csv", sep=",", dec=".", stringsAsFactors = FALSE)
TestNormal <- read.csv2("data/my_dataset_test.csv", sep=",", dec=".", stringsAsFactors = FALSE)


# TIPO 1: DATASET CON TODAS LAS VARIABLES A NUMÉRICAS


TrainSinNumericas <- TrainNormal
TestSinNumericas <- TestNormal

#Pasamos los "" de las categóricas a NA


TrainSinNumericas$x0[which(TrainSinNumericas$x0=="")]<-NA
TrainSinNumericas$x14[which(TrainSinNumericas$x14=="")]<-NA 
TrainSinNumericas$x17[which(TrainSinNumericas$x17=="")]<-NA 
TrainSinNumericas$x51[which(TrainSinNumericas$x51=="")]<-NA 
TrainSinNumericas$x61[which(TrainSinNumericas$x61=="")]<-NA 
TrainSinNumericas$x63[which(TrainSinNumericas$x63=="")]<-NA 


TestSinNumericas$x0[which(TestSinNumericas$x0=="")]<-NA
TestSinNumericas$x14[which(TestSinNumericas$x14=="")]<-NA 
TestSinNumericas$x17[which(TestSinNumericas$x17=="")]<-NA 
TestSinNumericas$x51[which(TestSinNumericas$x51=="")]<-NA 
TestSinNumericas$x61[which(TestSinNumericas$x61=="")]<-NA 
TestSinNumericas$x63[which(TestSinNumericas$x63=="")]<-NA 


#Pasamos a 0 1 en las variables binarias


TrainSinNumericas$x63<-ifelse(TrainSinNumericas$x63=="active",1,0)
TrainSinNumericas$x14<-ifelse(TrainSinNumericas$x14=="true",1,0)
TrainSinNumericas$x51<-ifelse(TrainSinNumericas$x51=="yes",1,0)

TestSinNumericas$x63<-ifelse(TestSinNumericas$x63=="active",1,0)
TestSinNumericas$x14<-ifelse(TestSinNumericas$x14=="true",1,0)
TestSinNumericas$x51<-ifelse(TestSinNumericas$x51=="yes",1,0)


#Eliminamos los strings iniciales y pasamos a numerico

TrainSinNumericas$x17<-substring(TrainSinNumericas$x17,4,5)
TrainSinNumericas$x61<-substring(TrainSinNumericas$x61,4,5)


TestSinNumericas$x17<-substring(TestSinNumericas$x17,4,5)
TestSinNumericas$x61<-substring(TestSinNumericas$x61,4,5)



#Tenemos una variable categórica que indica las tallas, podemos pasarla siguiendo el orden a numerica


TrainSinNumericas$x0[TrainSinNumericas$x0=="L"]<-4
TrainSinNumericas$x0[TrainSinNumericas$x0=="M"]<-3
TrainSinNumericas$x0[TrainSinNumericas$x0=="S"]<-2
TrainSinNumericas$x0[TrainSinNumericas$x0=="VL"]<-5
TrainSinNumericas$x0[TrainSinNumericas$x0=="VS"]<-1



TestSinNumericas$x0[TestSinNumericas$x0=="L"]<-4
TestSinNumericas$x0[TestSinNumericas$x0=="M"]<-3
TestSinNumericas$x0[TestSinNumericas$x0=="S"]<-2
TestSinNumericas$x0[TestSinNumericas$x0=="VL"]<-5
TestSinNumericas$x0[TestSinNumericas$x0=="VS"]<-1


#Tenemos una variable categórica que indica las tallas, podemos pasarla siguiendo el orden a numerica


TrainSinNumericas$x0  <- as.integer(TrainSinNumericas$x0)
TrainSinNumericas$x14 <- as.integer(TrainSinNumericas$x14)
TrainSinNumericas$x17 <- as.integer(TrainSinNumericas$x17)
TrainSinNumericas$x51 <- as.integer(TrainSinNumericas$x51)
TrainSinNumericas$x61 <- as.integer(TrainSinNumericas$x61 )
TrainSinNumericas$x63 <- as.integer(TrainSinNumericas$x63)

TestSinNumericas$x0  <- as.integer(TestSinNumericas$x0)
TestSinNumericas$x14 <- as.integer(TestSinNumericas$x14)
TestSinNumericas$x17 <- as.integer(TestSinNumericas$x17)
TestSinNumericas$x51 <- as.integer(TestSinNumericas$x51)
TestSinNumericas$x61 <- as.integer(TestSinNumericas$x61 )
TestSinNumericas$x63 <- as.integer(TestSinNumericas$x63)

# Escribimos los datos

write.csv2(TrainSinNumericas, "data/trainSinNumericos.csv", row.names = FALSE)
write.csv2(TestSinNumericas, "data/testSinNumericos.csv", row.names = FALSE)


# TIPO 2: IMPUTACIÓN POR NORM

# Copiamos los datos

TrainSinNumericasMVNorm <- TrainSinNumericas
TestSinNumericasMVNorm <- TestSinNumericas

# Imputamos por norm

mice_mod <- mice(TrainSinNumericasMVNorm[,colnames(TrainSinNumericasMVNorm)], method='norm')
TrainSinNumericasMVNorm <- complete(mice_mod)
mice_mod <- mice(TestSinNumericasMVNorm[,colnames(TestSinNumericasMVNorm)], method='norm')
TestSinNumericasMVNorm <- complete(mice_mod)

TrainSinNumericasMVNorm <- TrainSinNumericasMVNorm[,-42]
TestSinNumericasMVNorm  <- TestSinNumericasMVNorm[,-42]

write.csv2(TrainSinNumericasMVNorm, "data/trainSinNumericosMVNorm.csv", row.names = FALSE)
write.csv2(TestSinNumericasMVNorm, "data/testSinNumericosMVNorm.csv",row.names = FALSE)



# TIPO 3: IMPUTACIÓN POR MEDIA

TrainSinNumericasMVQuadratic <- TrainSinNumericas
TestSinNumericasMVQuadratic <- TestSinNumericas

# Imputamos por norm

mice_mod <- mice(TrainSinNumericasMVQuadratic[,colnames(TrainSinNumericasMVQuadratic)], method='quadratic')
TrainSinNumericasMVQuadratic <- complete(mice_mod)
mice_mod <- mice(TestSinNumericasMVQuadratic[,colnames(TestSinNumericasMVQuadratic)], method='quadratic')
TestSinNumericasMVQuadratic <- complete(mice_mod)


TrainSinNumericasMVQuadratic <- TrainSinNumericasMVQuadratic[,-42]
TestSinNumericasMVQuadratic  <- TestSinNumericasMVQuadratic[,-42]

write.csv2(TrainSinNumericasMVQuadratic, "data/trainSinNumericosMVQuadratic.csv", row.names = FALSE)
write.csv2(TestSinNumericasMVQuadratic, "data/testSinNumericosMVQuadratic.csv", row.names = FALSE)




