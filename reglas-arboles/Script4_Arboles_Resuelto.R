library(tree)

summary(iris)

# Construir un arbol que clasifica la especie en base al resto de variables
tree.iris = tree(Species~.,iris)

summary(tree.iris)

plot(tree.iris)
text(tree.iris, pretty=0)

tree.iris

# Dividir en training y test
set.seed (2)
train=sample (1:nrow(iris), 100)
iris.test=iris [-train ,]

# Construyo el arbol sobre el conjunto de entrenamiento
tree.iris =tree(Species~. ,iris ,subset =train )

# Aplico el arbol sobre el conjunto de test
tree.pred =predict (tree.iris ,iris.test ,type ="class")

# Visualizo la matriz de confusion
table(tree.pred , iris.test[,5])


# Podar el arbol usando cv

set.seed (3)
cv.iris = cv.tree(tree.iris ,FUN=prune.misclass )
names(cv.iris )
cv.iris

# Pintamos el error
par(mfrow =c(1,2))
plot(cv.iris$size ,cv.iris$dev ,type="b")
plot(cv.iris$k ,cv.iris$dev ,type="b")

# Ahora podamos el arbol con prune.misclass
prune.iris =prune.misclass (tree.iris ,best =3)
par(mfrow =c(1,1))
plot(prune.iris)
text(prune.iris ,pretty =0)

# Como se comportara este arbol en su capacidad de prediccion
tree.pred=predict (prune.iris , iris.test ,type="class")
table(tree.pred ,iris.test[,5])

# Ahora podemos modificar el tamanio del arbol modificando best
prune.iris =prune.misclass (tree.iris ,best =4)
plot(prune.iris)
text(prune.iris ,pretty =0)
tree.pred=predict (prune.iris , iris.test ,type="class")
table(tree.pred ,iris.test[,5])


# Random Forest
library (randomForest)
set.seed (1)
bag.iris = randomForest(Species~., data=iris, subset=train)
bag.iris

yhat.bag = predict (bag.iris ,newdata =iris.test)
yhat.bag


# Construyo una funcion para calcular el acierto a partir del RandomForest
acierto <- function(bag.datos){
  return (sum (sapply(1:length(bag.datos$y), function(x){
    if (is.na(bag.datos$predicted[x])){
      0
    } 
    else if (as.numeric(bag.datos$y[x])==as.numeric(bag.datos$predicted[x])){
      1
    }
    else{
      0
    }
  }))/length(bag.datos$y))
}

resul = as.data.frame(cbind(predicted = yhat.bag, y=iris.test[,5]))
acierto(resul)


# Fijando el numero de arboles
bag.iris = randomForest(Species~.,data=iris ,subset =train , ntree=25)
bag.iris



# Experimento: Graficar la evolucion incrementando el numero de arboles


# construyo un funcion para obtener la evolucion del acierto en base
# al numero de arboles en randomForest

Graphical_RF <- function(datos, formula, num_trees){
  train=sample (1:nrow(datos), 2*(dim(datos)[1]/3))
  datos.test=datos [-train ,]
  grafica = sapply(1:num_trees, function(x){
    bag.datos = randomForest(as.formula(formula), data=datos, subset=train, ntree = x, na.action=na.omit)
    acierto(bag.datos)
  })
  # Pinto la evolucion
  plot(1:num_trees, grafica, type="b", col ="red", xlab="number of trees", ylab="Accuracy on Test")

  return (grafica)
}


Graphical_RF(iris,Species~.,1000)

# Ejercicio: Aplicar la funcion anterior a la base de datos "Auto"

library(ISLR)

# Adapto Auto para aplicar la funcion anterior, y pongo como
# variable dependiente origin

datos = Auto
datos[,8] = as.factor(datos[,8])

Graphical_RF(datos,origin~.,100)
# Da un error, ya que esta implementacion de RandomForest no puede
# manejar variables que tomen mas de 53 valores diferentes para
# variables enteras o nominales. 
# El problema esta en la variable "names" que tiene mas de 300 valores 
# nominales distintos.
# Eliminamos esa variable.

Graphical_RF(datos,origin~.-name,100)



 ##################################################################
#                     Boosting                                   #
#################################################################
library (gbm)
set.seed (1)

boost.iris =gbm(Species~.,data=iris[train,], 
                distribution="multinomial",n.trees =5000,
                interaction.depth =4)

summary(boost.iris)


yhat.boost=predict (boost.iris ,newdata =iris[-train,],
                    n.trees =5000)

yhat.boost

# Obtenemos el porcentaje de acierto
yhat.boost = matrix(yhat.boost,ncol=(length(yhat.boost)/nrow(iris.test)))

yhat.boost.y = sapply(1:nrow(iris.test), function(x){
  which.max(yhat.boost[x,])
})

(sum(yhat.boost.y == as.numeric(iris.test[,5]))/nrow(iris.test))*100



 ##################################################################
#          Usando RWeka y C4.5
##################################################################

library(RWeka)

# La biblioteca RWeka implementa algunos algoritmos de clasificacion
# basados en arboles de decision.

# El mas conocido de ellos es C4.5
?J48

# J48 es la implementacion de C4.5 en Weka, y su uso es similar
# al de la funcion "tree" vista anteriormente en este script.
# Vemos un ejemplo de uso sobre "iris"


modelC4.5 = J48(Species~., data=iris, subset=train)

modelC4.5.pred = predict(modelC4.5, iris.test)

library(partykit)
plot(modelC4.5)
modelC4.5.pred

resul = as.data.frame(cbind(predicted = modelC4.5.pred, y=iris.test[,5]))

acierto(resul)



# Si queremos hacer una validacion cruzada usando RWeka para C4.5 y
# la base de datos "iris", se haria de la siguiente forma

modelC4.5 = J48(Species~., data=iris)
cv_resul = evaluate_Weka_classifier(modelC4.5,numFolds=10)
cv_resul


# RWeka tambien proporciona 3 algoritmos mas
# LMT implementa "Logistic Model Trees"
# M5P una version mejorada de C4.5
# DecisionStump arboles con solo un nodo decisor (multiclasificadores/Boosting)

