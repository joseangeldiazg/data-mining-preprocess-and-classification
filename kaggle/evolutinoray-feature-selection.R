#José Ángel

#**********************************************
#Experimento con seleccion de variables usando algoritmo evolutivo
#**********************************************

#Librerias
library(caret)
library(NoiseFiltersR)

#**********************************************
# Carga de datos
#**********************************************

Train <- read.csv2("data/Numericos_ImpKNN_SinCorr-TRAIN.csv", stringsAsFactors = FALSE)
Test <- read.csv2("data/Numericos_ImpKNN_SinCorr-TEST.csv", stringsAsFactors = FALSE)

# Las copio a variables nuevas para trabajar sobre ellas

train <- Train
test <- Test

#**********************************************
# Selección de variables más relevantes
#**********************************************

## Not run: 
set.seed(1)

ctrl <- gafsControl(functions = rfGA,
                    method = "cv",
                    number = 3)

rf_search <- gafs(x = train[, -ncol(train)],
                  y = as.factor(train$y),
                  iters = 3,
                  gafsControl = ctrl)

rf_search
stopCluster(cluster)

