suppressWarnings(suppressMessages(library(kknn)))
suppressWarnings(suppressMessages(library(e1071)))
suppressWarnings(suppressMessages(library(class)))
suppressWarnings(suppressMessages(library(rpart)))
suppressWarnings(suppressMessages(library(randomForest)))
suppressWarnings(suppressMessages(library(ada)))
suppressWarnings(suppressMessages(library(ROCR)))
suppressWarnings(suppressMessages(library(caret)))
suppressWarnings(suppressMessages(library(ggplot2))

# Para esta pregunta usaremos los datos CompraBicicletas.csv, esta tabla contiene 11 variables predictoras y el PurchasedBike que es la variable a predecir, la cual indica si un cliente compro o no una bicicleta.

purchased.bike <- read.table("Data/purchasedBikes.csv", header = TRUE, sep = ";", dec = ",", row.names =  1)

n <- dim(purchased.bike)[1] #Numero de Filas

dim(purchased.bike)

summary(purchased.bike)

#El objetivo de este ejercicio es comparar todos los metodos predictivos vistos en el curso con esta tabla de datos. Aqui interesa predecir el Yes en la variable PurchasedBike, para esto genere 10 Validaciones Cruzadas con 5 grupos para los metodos SVM, KNN, Bayes, Arboles, Bosques, Potenciacion y Redes Neuronales. Luego grafique las 10 iteraciones para todos los metodos en el mismo grafico. Se puede determinar con claridad cual metodos es el mejor?
  
#Repita el ejercicio anterior, pero esta vez en lugar de sumar los Yes detectados, promedie los errores globales cometidos en los diferentes grupos (folds). Luego grafique las 10 itereaciones para los tres algoritmos en el mismo grafico. Se puede determinar con claridad cual algoritmo es el mejor?

###############
# para determinar la cantidad de Yes en la variable PurchasedBike y el promedio de los errores globales, ejecutamos el siguiente codigo:


#SVM
deteccion.yes.svm <- rep(0,10)
deteccion.error.svm <- rep(0,10)

#KNN
deteccion.yes.knn <- rep(0,10)
deteccion.error.knn <- rep(0,10)

#Bayes
deteccion.yes.bayes <- rep(0,10)
deteccion.error.bayes <- rep(0,10)

#Arboles
deteccion.yes.arbol <- rep(0,10)
deteccion.error.arbol <- rep(0,10)

#Bosques
deteccion.yes.bosque<-rep(0,10)
deteccion.error.bosque<-rep(0,10)

#Potenciacion
deteccion.yes.potenciacion<-rep(0,10)
deteccion.error.potenciacion<-rep(0,10)

#Red Neuronal
##deteccion.yes.red<-rep(0,10)
##deteccion.error.red<-rep(0,10)


# Validación cruzada 10 veces
for(i in 1:10) {
  grupos <- createFolds(1:n,5) # Crea los 5 grupos
  
  #SVM
  yes.svm <- 0
  error.svm <- 0
  
  #KNN
  yes.knn <- 0
  error.knn <- 0
  
  #Bayes
  yes.bayes <- 0
  error.bayes <- 0
  
  #Arboles
  yes.arbol <- 0
  error.arbol <- 0
  
  #Bosques
  yes.bosque <- 0
  error.bosque <- 0
  
  #Potenciacion
  yes.potenciacion <- 0
  error.potenciacion <- 0
  
  #Red Neuronal
  ##si.red <- 0
  ##error.red <- 0
  
  # Este ciclo es el que hace "cross-validation" (validación cruzada) con 5 grupos (Folds)
  for(k in 1:5) {    
    
    training  <- grupos[[k]] # Por ser una lista requiere de doble paréntesis
    ttesting  <- purchased.bike[training,]
    tlearning <- purchased.bike[-training,]
    
    #SVM
    modelo      <- svm(PurchasedBike~., data = tlearning, kernel ="radial")
    prediccion  <- predict(modelo, ttesting)
    Actual      <- ttesting[, 12]
    MC          <- table(Actual, prediccion)
    
    # Detección de los SI compradores
    yes.svm <- yes.svm + MC[2,2]
    # Detección del ERROR
    error.svm <- error.svm + (1 - (sum(diag(MC)))/sum(MC))*100
    
    #KNN
    modelo     <- train.kknn(PurchasedBike~., data = tlearning, kmax = 7)
    prediccion <- predict(modelo,ttesting[, -12])
    Actual     <- ttesting[, 12]
    MC         <- table(Actual, prediccion)
    
    # Detección de los SI compradores
    yes.knn <- yes.knn + MC[2,2]
    # Detección del ERROR
    error.knn <- error.knn + (1 - (sum(diag(MC)))/sum(MC))*100
    
    #Bayes
    modelo     <- naiveBayes(PurchasedBike~., data = tlearning)
    prediccion <- predict(modelo, ttesting[,-12])
    Actual     <- ttesting[,12]
    MC         <- table(Actual, prediccion)
    
    #Detección de los SI compradores
    yes.bayes <- yes.bayes + MC[2,2]
    # Detección del ERROR
    error.bayes <- error.bayes + (1 - (sum(diag(MC)))/sum(MC))*100
    
    #Arboles
    modelo     <- rpart(PurchasedBike~. ,data = tlearning)
    prediccion <- predict(modelo, ttesting, type='class')
    Actual     <- ttesting[, 12]
    MC         <- table(Actual, prediccion)
    
    #Detección de los SI compradores
    yes.arbol <- yes.arbol + MC[2,2]
    # Detección del ERROR
    error.arbol <- error.arbol + (1 - (sum(diag(MC)))/sum(MC))*100
    
    #Bosques
    modelo     <- randomForest(PurchasedBike~., data = tlearning, importance=TRUE)
    prediccion <- predict(modelo, ttesting[, -12])
    Actual     <- ttesting[, 12]
    MC         <- table(Actual, prediccion)
    
    #Detección de los SI compradores
    yes.bosque <- yes.bosque + MC[2,2]
    # Detección del ERROR 
    error.bosque <- error.bosque + (1 - (sum(diag(MC)))/sum(MC))*100
    
    #Potenciacion
    modelo     <- ada(PurchasedBike~., data = tlearning, iter=20, nu = 1, type = "discrete")
    prediccion <- predict(modelo, ttesting[, -12])
    Actual     <- ttesting[, 12]
    MC         <- table(Actual, prediccion)
    
    #Detección de los SI compradores
    yes.potenciacion <- yes.potenciacion + MC[2,2]
    # Detección del ERROR 
    error.potenciacion <- error.potenciacion + (1-(sum(diag(MC)))/sum(MC))*100
    
    #Red Neuronal
    ##modelo<-nnet(PurchasedBike~.,data=tlearning,size=5,rang=0.1,decay=5e-4,maxit=100,trace=FALSE)
    ##prediccion<-predict(modelo, ttesting[,-12],type = "class")
    ##Actual<-ttesting[,12]
    ##MC<-table(Actual,prediccion)
    # Detección de los SI compradores
    ##MC <- MatrizConfusionRN(MC)
    ##si.red <- si.red + MC[2,2]
    # Detección del ERROR 
    ## error.red<-error.red + (1-(sum(diag(MC)))/sum(MC))*100
  }
  
  #SVM  
  deteccion.yes.svm[i]   <- yes.svm
  deteccion.error.svm[i] <- error.svm/5
  
  #KNN
  deteccion.yes.knn[i]   <- yes.knn
  deteccion.error.knn[i] <- error.knn/5
  
  #Bayes
  deteccion.yes.bayes[i]   <- yes.bayes
  deteccion.error.bayes[i] <- error.bayes/5
  
  #Arboles
  deteccion.yes.arbol[i]   <- yes.arbol
  deteccion.error.arbol[i] <- error.arbol/5
  
  #Bosque
  deteccion.yes.bosque[i]   <- yes.bosque
  deteccion.error.bosque[i] <- error.bosque/5
  
  #Potenciacion
  deteccion.yes.potenciacion[i]   <- yes.potenciacion
  deteccion.error.potenciacion[i] <- error.potenciacion/5
  
  #Red Neuronal
  #deteccion.yes.red[i] <- si.red
  #deteccion.error.red[i] <- error.red/5
}
# PLOT DE DETECCION DE SI COMPRADORES
plot(deteccion.yes.svm, col = "magenta", type = "b",  ylim = c(min(deteccion.yes.svm,deteccion.yes.knn,deteccion.yes.bayes,deteccion.yes.arbol,deteccion.yes.bosque,deteccion.yes.potenciacion), max(deteccion.yes.svm,deteccion.yes.knn,deteccion.yes.bayes,deteccion.yes.arbol,deteccion.yes.bosque,deteccion.yes.potenciacion)+0.05), main = "Yes purchase detection", xlab = "Número de iteración", ylab = "Quantity of yes purchased detection")
points(deteccion.yes.knn, col = "blue", type = "b")
points(deteccion.yes.bayes, col = "red", type = "b")
points(deteccion.yes.arbol, col = "lightblue3", type = "b")
points(deteccion.yes.bosque, col = "olivedrab", type = "b")
points(deteccion.yes.potenciacion, col = "orange3", type = "b")
#points(deteccion.yes.red, col = "rosybrown4", type = "b")
legend("topright", legend = c("SVM","KNN","Bayes","Árbol","Bosque","Potenciación"), col = c("magenta", 
                                                                                                           "blue","red","lightblue3","olivedrab","orange3"), lty = 1, lwd = 1)

# En este caso, vemos como el modelo de Bosques aleatorios es el que tiene una mayor deteccion de los si compradores en las 10 iteraciones solo proximado por el metodo de KNN en la iteracion numero 6. Para el caso delo error global el plot se puede obtener ejecutando el siguiente codigo:


plot(deteccion.error.svm, col = "magenta", type = "b",  ylim = c(min(deteccion.error.svm,deteccion.error.knn,deteccion.error.bayes,deteccion.error.arbol,deteccion.error.bosque,deteccion.error.potenciacion), max(deteccion.error.svm,deteccion.error.knn,deteccion.error.bayes,deteccion.error.arbol,deteccion.error.bosque,deteccion.error.potenciacion)+3), main = "Detección del ERROR", xlab = "Número de iteración", ylab = "ERROR Cometido")
points(deteccion.error.knn, col = "blue", type = "b")
points(deteccion.error.bayes, col = "red", type = "b")
points(deteccion.error.arbol, col = "lightblue3", type = "b")
points(deteccion.error.bosque, col = "olivedrab", type = "b")
points(deteccion.error.potenciacion, col = "orange3", type = "b")
legend("topright", legend = c("SVM","KNN","Bayes","Árbol","Bosque","Potenciación"), col = c("magenta", "blue","red","lightblue3","olivedrab","orange3"), lty = 1, lwd = 2)
