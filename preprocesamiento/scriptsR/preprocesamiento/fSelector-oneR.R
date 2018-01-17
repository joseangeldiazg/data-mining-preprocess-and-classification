library(FSelector)
data(HouseVotes84)

# se calculan los pesos
weights <- FSelector::oneR(Class~.,HouseVotes84)

# se muestran los resultados
print(weights)
subset <- cutoff.k(weights,5)
f <- as.simple.formula(subset,"Class")
print(f)
