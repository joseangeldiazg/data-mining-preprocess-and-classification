library(outliers)
library(ggplot2)

# se carga el archivo con las funcione de lectura de datos
source("lecturaDatos.R")

path <- "../"
file <- "my_dataset_train.csv"

# lectura de los datos
datos <- lecturaDatos(path,file)

# deteccion de anomalias para las variable 1 a 3. Observad 
# que no tiene sentido considerar variables de tipo discreto
# en este analisis. La funcion devuelve el valor (o valores)
# considerados anomalos para las variable de interes. Este
# metodo solo considera las desviaciones con respecto a los
# valores de cada variable (no relaciones con otras variables)
anomalos <- outlier(datos[,2:4])
print(anomalos)

# la media de la variable x3 es
mean(datos[,"x3"], na.rm = TRUE)

# se muestra la distribucion de x3 en funcion del valor
# de la variable clase
ggplot(data = datos, aes(y, x3)) +
  geom_boxplot()

# se podria hacer igual con la variable x2
ggplot(data = datos, aes(y, x2)) +
  geom_boxplot()

 
