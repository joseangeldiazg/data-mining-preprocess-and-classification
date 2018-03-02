#Jose Angel

#Votación por mayoría de todos los scripts de kaggle: La idea es obtener las mejores solucioenes de kaggle, combinarlas y por mayoria asignar la clase



#**********************************************
# Lectura de ficheros
#**********************************************

setwd("./sub083omas")
temp = list.files(pattern="*.csv")
#Leemos los ficheros
myfiles = lapply(temp, read.delim, sep=",")
setwd("../")


#**********************************************
# Creación del dataframe
#**********************************************
myfiles[[1]]$Prediction
matriz.predicciones<-1:683

for(i in 1:length(myfiles))
{
  matriz.predicciones<-cbind(matriz.predicciones,myfiles[[i]]$Prediction)
}
matriz.predicciones<-as.data.frame(matriz.predicciones[,2:33])


#**********************************************
# Obtencion de votaciones
#**********************************************

#Ahora creamos una lista con las votaciones de cada elemento

lista.votaciones<-apply(matriz.predicciones,MARGIN=1,table)

#Recorremos la lista y nos quedamos con la mayoritaria

resultado.votacion<-vector()

for(i in 1:length(lista.votaciones))
{
  resultado.votacion[i]<-names(which.min(lista.votaciones[[i]]))
}


resultado.votacion

predicciones <- as.integer(resultado.votacion)

# Construyo la matriz de predicciones
predicciones <- matrix(c(1:683,predicciones),683,2,byrow = FALSE)
colnames(predicciones) <- c("Id","Prediction")
# Guardo
write.table(predicciones, "sub/votacion-minoria.csv", col.names = TRUE, row.names = FALSE,
            quote=FALSE, sep = ",")

