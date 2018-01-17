library(FSelector)
data(iris)

# se calculan los pesos
weights <- relief(Species~.,iris, neighbours.count=5, sample.size=20)

# se muestran los resultados
print(weights)
subset <- cutoff.k(weights,2)
f <- as.simple.formula(subset,"Species")
print(f)
