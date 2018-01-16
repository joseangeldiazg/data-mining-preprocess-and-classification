#Pasos a seguir para la confeccion de un modelo lineal
# 1. Analisis preliminar (Grafico y Numerico)
# 2. Construccion del modelo
# 3. Estimacion de la bondad del modelo construido

# 0. Datos con los que vamos a trabajar

# Usaremos la base de datos "iris" sin la variable de clasificacion
library(ISLR)
datos <-data.frame(y=iris$Sepal.Width,x1=iris$Sepal.Length,x2=iris$Petal.Length,x3=iris$Petal.Width)

Yreal = iris$Sepal.Width
X = cbind(1,iris$Sepal.Length,iris$Petal.Length,iris$Petal.Width)



# Objetivo: Construir un modelo lineal que aproxime "y" a partir de "x1","x2" y "x3"
# 1.a. Analisis preliminar (Grafico)

plot(datos)

# Resultado Evaluacion: A la vista de las grafica ninguna de las variables parece prometedora.


# 1.b. Analisis preliminar (Numerico)
#      Correlaciones Simples
#         Valores cercanos a 1 y -1 indican alta correlacion entre 
#         las variables. Valores cercanos a 0 indican correlacion 
#         baja o no correlacion.

cor(datos)

# Resultado Evaluacion: Se confirma la aparente no correlación con la variable de salida



# 2. Construccion del modelo

reg_lineal <-lm(y~x1+x2+x3, data= datos)
resumen_reg_lineal <-summary(reg_lineal)
resumen_reg_lineal


# Resultado Evaluacion: El modelo es 
#           y= -1.04309+0.0.60707 x1 - 0.58603 x2 + 0.55803 x3
#


#Contraste de hipotesis
# Pregunta: H0 Bi=0 para todo i del modelo puntual
# De la columna Pr(>|t|) deducimos que todas las variables son significativas


# 3. Estimacion de la bondad del modelo construido

# Vamos a tener en cuenta 7 criterios.
# a) Estimacion del Sigma (Error Estandar Residual [EER])
#    Se mira en la tabla "summary" y el valor es 0.3038
#    Calculamos el coeficiente de variacion con la siguiente forma

cv <-100*(resumen_reg_lineal$sigma/(mean(datos$y)))
cv

# Calculo manual del EER

n = length(Yreal)
p = length(reg_lineal) # Numero de parametros de la aproximacion
Ypred = predict(reg_lineal, data = datos)
vr = sum( (Ypred-Yreal) * (Ypred-Yreal) )
vr = vr * (1/(length(Yreal)-p))
vr = sqrt(vr)
cv <- 100*(vr/mean(Yreal))
cv


# Si el valor obtenido es inferior al 10%, se considera un error estandar
# residual aceptable. En nuestro caso es 9.93 < 10, por tanto, aceptable.





# b) Tabla ANOVA para responder a la siguiente hipotesis
# Pregunta: H0 B1 = B2 = .. = Bd = 0
# Se consulta en la tabla "summary" el valor p-value

resumen_reg_lineal

# El valor del p-value es inferior a 0.05 y por consiguiente, se rechaza H0.





# c) Coeficiente de determinacion (R cuadrado)
# Se mira "Multiple R cuadrado" que indica que porcentaje de la
# la variabilidad de los datos la explica el modelo

resumen_reg_lineal$r.squared

# El valor es 52.4%, inferior al 80% y por consiguiente no es aceptable.

# Se debe comparar con el "R cuadrado ajustado". Los valores deben ser 
# parecidos. Si no es asi, eso puede indicar que R cuadrado se ha visto
# afectado por un elevado numero de variables predictivas.

resumen_reg_lineal$adj.r.squared


# El calculo manual se hace de la siguiente forma
Ypred = predict(reg_lineal, data = datos)
n = length(Yreal)
p = length(reg_lineal)

VT = sum( (Yreal-mean(Yreal))*(Yreal-mean(Yreal)) )
VE = sum( (Ypred-mean(Yreal))*(Ypred-mean(Yreal)) )
VR = sum(  (Yreal-Ypred) * (Yreal-mean(Yreal)))

R2 = VE / VT
R2.corregido = 1 - (1-R2)*(n-1)/(n-p)


# En nuestro caso es de 51.4%, muy parecido al anterior y por tanto,
# nos creemos el valor del "Multiple R cuadrado".






# d) Calculo del Error Cuadratico Medio (MSE)
reg_lineal.fit <- predict(reg_lineal, newdata = datos)

sum(((datos$y-reg_lineal.fit)^2))/length(reg_lineal.fit)







# e) Normalidad

par(mfrow=c(2,2))
plot(reg_lineal)

# Miramos el grafico "Normal Q-Q" donde queremos que los datos esten lo mas
# proximos posibles a la linea discontinua. En nuestro caso parece
# que los errores se distribuyen segun una distribucion normal, aunque 
# los primeros valores parece que se alejan un poco de esta linea

# Para asegurar mas este diagnostico, haremos un histograma sobre los
# errores (residuos)

par(mfrow=c(1,1))

e <-residuals(reg_lineal)
d <- e/resumen_reg_lineal$sigma

hist (d, probability = T, xlab = "Errores estandar", main = "", xlim = c(-3,3))

d.seq <- seq(-3,3,length = 50)

lines(d.seq, dnorm(d.seq, mean(d), sd(d)), col="red")

# Un ultimo test para ver si los errores son normales
# El test de normalidad Shapiro-Wilk, tiene como H0: error~N(mean,sd)

shapiro.test(e)

# Al ser el p-value>0.05 no se puede rechazar la hipotesis de normalidad








# f) Homocedasticidad (hipotesis de varianza constante)

# Mirar el grafico de plot(reg_lineal) llamado "Residuals vs Fitted
# Se debe observar que la anchura de los datos es aproximadamente igual.
# Nos marca los ejemplos extremos que son el 135, 142 y 146.

# Usaremos el test de Breusch-Pagan que tiene como H0: homocedastidad

library(lmtest)
bptest(reg_lineal)

# El valor obtenido es de 0.865, mayor que 0.05 por lo que no se puede descartar la hipotesis de homocedasticidad







# g) Incorrelacion

n <- length(d)
plot(d[1:n-1],d[2:n],xlab = "Error i", ylab = "Error i-1")
lines(lowess(d[1:n-1],d[2:n]),col="red")

# No se ve ninguna tencendia creciente o decreciente en la recta, 
# lo que parece que la hipotesis se cumple.

# Saldremos de dudas con un contraste de hipotesis
# Test de Durbin-Watson, con H0: correlacion 0

library(lmtest)
dwtest(reg_lineal,alternative = "two.sided")

# p-value >0.05 no se puede rechar la hipotesis de correlacion 0. Por tanto,
# satisface la condicion de incorrelacion.
























############################### Visualizacion #####################################

# Visualizacion del ajuste
# a) Mateniendo el orden de la secuencia del conjunto
plot(1:dim(datos)[1],datos[,1],xlab="Ejemplos",ylab="y")
pred <- predict(reg_lineal, newdata = datos)
points(1:dim(datos)[1],pred, col="red")


# b) Ordenando los datos de menor a mayor valor de la variable de salida
datos_ord <-datos[sort(datos[,1], index.return=TRUE)$ix,]
reg_lineal = lm(y~x1+x2+x3, data= datos_ord)
plot(1:dim(datos_ord)[1],datos_ord[,1],xlab="Ejemplos", ylab="y")
pred <- predict(reg_lineal, newdata = datos_ord)
points(1:dim(datos_ord)[1],pred, col="red")

# Se pueden incluir las discrepancias usando la siguiente sentencia
segments(1:dim(datos_ord)[1], datos_ord$y, 1:dim(datos_ord)[1], pred,col="blue", lty = 2)

# Se puede mostrar la linea entre los ejemplos descomentando la siguiente sentencia
# lines(1:dim(datos_ord)[1],pred, col="red")



