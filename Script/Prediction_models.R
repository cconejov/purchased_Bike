
library(tidyverse)
library(kknn)
library(e1071)
library(class)
library(rpart)
library(randomForest)
library(ada)
library(ROCR)
library(caret)

# ML can be divided in two branch. Supervised and unsupervised methods.
# Focus in supervised, specially, clasification 

# Using the file  purchasedBikes.csv. This table contains 11 predictible variable and the binary class as output  PurchasedBike. It indicates if a customer buy or not a bycicle.


purchased_bike <- read.table("Data/purchasedBikes.csv", header = TRUE, sep = ";", dec = ",", row.names =  1)

# Detail
summary(purchased_bike)
pairs(purchased_bike)


# Balanced data
ggplot(data = purchased_bike) +
  geom_bar(aes( x = PurchasedBike, fill = PurchasedBike)) +
    labs(title = "Distribution Pirchased Bike")  



n <- dim(purchased_bike)[1] #Numero de Filas 100

# The goal of this exercise is to compare several clasification techiques with this example table. 
# Here, the purpose is to predict Yes value in the PurchasedBike using ten cross validation with 5 groups with the methods:

## Support Vector Machine
## KNN
## Bayes
## Decision tree
## Random Forest
## Boosting methods (AdaBoost)


# We will graph 10 iteraction por each one of this methods

# Also, we are going to see the average of the global error in the diferent groups 

#El objetivo de este ejercicio es comparar todos los metodos predictivos vistos en el curso con esta tabla de datos.
#Aqui interesa predecir el Yes en la variable PurchasedBike, para esto genere 10 Validaciones Cruzadas con 5 groups para 
#los metodos SVM, KNN, Bayes, treees, RFs, boosting y Redes Neuronales. 
#Luego grafique las 10 iteraciones para todos los metodos en el mismo grafico. 
#Se puede determinar con claridad cual metodos es el mejor?
  
#Repita el ejercicio anterior, pero esta vez en lugar de sumar los Yes detectados, promedie los errores globales cometidos en los diferentes groups (folds). 

#Luego grafique las 10 itereaciones para los tres algoritmos en el mismo grafico. Se puede determinar con claridad cual algoritmo es el mejor?

###############
# para determinar la cantidad de Yes en la variable PurchasedBike y el promedio de los errores globales, ejecutamos el siguiente codigo:


# Determine the number of "yes" in the PurchasedBike
# Average of global error


#SVM
yes_detect_svm <- rep(0,10)
error_detect_svm <- rep(0,10)

#KNN
yes_detect_knn <- rep(0,10)
error_detect_knn <- rep(0,10)

#Bayes
yes_detect_bayes <- rep(0,10)
error_detect_bayes <- rep(0,10)

#trees
yes_detect_tree <- rep(0,10)
error_detect_tree <- rep(0,10)

#RFs
yes_detect_RF<-rep(0,10)
error_detect_RF<-rep(0,10)

#boosting
yes_detect_boosting<-rep(0,10)
error_detect_boosting<-rep(0,10)

#Red Neuronal
##yes_detect_red<-rep(0,10)
##error_detect_red<-rep(0,10)


# cross validation 10 times
for(i in 1:10) {  #10
  groups <- createFolds(1:n,5) #5 groups
  
  #SVM
  yes_svm   <- 0
  error_svm <- 0
  auc_svm   <- 0
  
  #KNN
  yes_knn <- 0
  error_knn <- 0
  
  #Bayes
  yes_bayes <- 0
  error_bayes <- 0
  
  #treees
  yes_tree <- 0
  error_tree <- 0
  
  #RFs
  yes_RF <- 0
  error_RF <- 0
  
  #boosting
  yes_boosting <- 0
  error_boosting <- 0
  
  #Red Neuronal
  ##si.red <- 0
  ##error_red <- 0
  
  # This for does "cross-validation"  with 5 groups (Folds)
  for(k in 1:5) {   #5 
    
    training  <- groups[[k]]              #list
    ttesting  <- purchased_bike[training,]
    tlearning <- purchased_bike[-training,]
    
    #SVM
    model      <- svm(PurchasedBike~., data = tlearning, kernel ="radial")
    prediction  <- predict(model, ttesting)
    Actual      <- ttesting[, 12]
    MC          <- table(Actual, prediction)
    
    # Detección de los SI compradores
    yes_svm <- yes_svm + MC[2,2]
    # Detección del ERROR
    error_svm <- error_svm + (1 - (sum(diag(MC)))/sum(MC))*100

    #KNN
    model     <- train.kknn(PurchasedBike~., data = tlearning, kmax = 7)
    prediction <- predict(model,ttesting[, -12])
    Actual     <- ttesting[, 12]
    MC         <- table(Actual, prediction)
    
    # Detección de los SI compradores
    yes_knn <- yes_knn + MC[2,2]
    # Detección del ERROR
    error_knn <- error_knn + (1 - (sum(diag(MC)))/sum(MC))*100
    
    #Bayes
    model     <- naiveBayes(PurchasedBike~., data = tlearning)
    prediction <- predict(model, ttesting[,-12])
    Actual     <- ttesting[,12]
    MC         <- table(Actual, prediction)
    
    #Detección de los SI compradores
    yes_bayes <- yes_bayes + MC[2,2]
    # Detección del ERROR
    error_bayes <- error_bayes + (1 - (sum(diag(MC)))/sum(MC))*100
    
    #treees
    model     <- rpart(PurchasedBike~. ,data = tlearning)
    prediction <- predict(model, ttesting, type='class')
    Actual     <- ttesting[, 12]
    MC         <- table(Actual, prediction)
    
    #Detección de los SI compradores
    yes_tree <- yes_tree + MC[2,2]
    # Detección del ERROR
    error_tree <- error_tree + (1 - (sum(diag(MC)))/sum(MC))*100
    
    #RFs
    model     <- randomForest(PurchasedBike~., data = tlearning, importance=TRUE)
    prediction <- predict(model, ttesting[, -12])
    Actual     <- ttesting[, 12]
    MC         <- table(Actual, prediction)
    
    #Detección de los SI compradores
    yes_RF <- yes_RF + MC[2,2]
    # Detección del ERROR 
    error_RF <- error_RF + (1 - (sum(diag(MC)))/sum(MC))*100
    
    #boosting
    model     <- ada(PurchasedBike~., data = tlearning, iter=20, nu = 1, type = "discrete")
    prediction <- predict(model, ttesting[, -12])
    Actual     <- ttesting[, 12]
    MC         <- table(Actual, prediction)
    
    #Detección de los SI compradores
    yes_boosting <- yes_boosting + MC[2,2]
    # Detección del ERROR 
    error_boosting <- error_boosting + (1-(sum(diag(MC)))/sum(MC))*100
    
    #Red Neuronal
    ##model<-nnet(PurchasedBike~.,data=tlearning,size=5,rang=0.1,decay=5e-4,maxit=100,trace=FALSE)
    ##prediction<-predict(model, ttesting[,-12],type = "class")
    ##Actual<-ttesting[,12]
    ##MC<-table(Actual,prediction)
    # Detección de los SI compradores
    ##MC <- MatrizConfusionRN(MC)
    ##si.red <- si.red + MC[2,2]
    # Detección del ERROR 
    ## error_red<-error_red + (1-(sum(diag(MC)))/sum(MC))*100
  }
  
  #SVM  
  yes_detect_svm[i]   <- yes_svm
  error_detect_svm[i] <- error_svm/5
  
  #KNN
  yes_detect_knn[i]   <- yes_knn
  error_detect_knn[i] <- error_knn/5
  
  #Bayes
  yes_detect_bayes[i]   <- yes_bayes
  error_detect_bayes[i] <- error_bayes/5
  
  #treees
  yes_detect_tree[i]   <- yes_tree
  error_detect_tree[i] <- error_tree/5
  
  #RF
  yes_detect_RF[i]   <- yes_RF
  error_detect_RF[i] <- error_RF/5
  
  #boosting
  yes_detect_boosting[i]   <- yes_boosting
  error_detect_boosting[i] <- error_boosting/5
  
  #Red Neuronal
  #yes_detect_red[i] <- si.red
  #error_detect_red[i] <- error_red/5
}

# En este caso, vemos como el model de RFs aleatorios es el que tiene una mayor deteccion de los si compradores en las 10 iteraciones solo proximado por el metodo de KNN en la iteracion numero 6. Para el caso delo error global el plot se puede obtener ejecutando el siguiente codigo:

colors <- c("SVM"      = "blue", 
            "KNN"      = "red",
            "Bayes"    =  "orange",
            "Tree"     = "purple",
            "RF"       = "black",
            "Boost" = "green"
)

detect <- tibble(Iteration = seq(1,10), 
                 SVM = yes_detect_svm, 
                 KNN = yes_detect_knn,
                 Bayes = yes_detect_bayes,
                 Tree = yes_detect_tree,
                 RF   = yes_detect_RF,
                 Boost = yes_detect_boosting)

ggplot(data = detect, aes(x = Iteration) ) +
  geom_line(aes( y = SVM, color = "SVM") ) +
  geom_line(aes( y = KNN, color = "KNN")) +
  geom_line(aes( y = Bayes, color = "Bayes")) +
  geom_line(aes( y = Tree, color = "Tree")) +
  geom_line(aes( y = RF, color = "RF"), size = 1) +
  geom_line(aes( y = Boost, color = "Boost")) +
  scale_x_continuous(breaks=c(1:10), labels=c(1:10),limits=c(1,10)) +
  labs(x = "Iteration",
       y = "Yes detection",
       color = "Method",
       title = "Quantity of Purchased bike variable"
       ) +
  scale_color_manual(values = colors)

############################
# Global error
############################

global_error <- tibble(Iteration = seq(1,10), 
                 SVM = error_detect_svm, 
                 KNN = error_detect_knn,
                 Bayes = error_detect_bayes,
                 Tree = error_detect_tree,
                 RF   = error_detect_RF,
                 Boost = error_detect_boosting)


ggplot(data = global_error, aes(x = Iteration) ) +
  geom_line(aes( y = SVM, color = "SVM") ) +
  geom_line(aes( y = KNN, color = "KNN")) +
  geom_line(aes( y = Bayes, color = "Bayes")) +
  geom_line(aes( y = Tree, color = "Tree")) +
  geom_line(aes( y = RF, color = "RF"), size = 1) +
  geom_line(aes( y = Boost, color = "Boost")) +
  scale_x_continuous(breaks=c(1:10), labels=c(1:10),limits=c(1,10)) +
  labs(x = "Iteration",
       y = "AVG global Error",
       color = "Method",
       title = "Average global error by iteration"
  ) +
  scale_color_manual(values = colors)

## PLOT R BASE
# PLOT DE DETECCION DE SI COMPRADORES
plot(yes_detect_svm, col = "magenta", type = "b",  ylim = c(min(yes_detect_svm,yes_detect_knn,yes_detect_bayes,yes_detect_tree,yes_detect_RF,yes_detect_boosting), max(yes_detect_svm,yes_detect_knn,yes_detect_bayes,yes_detect_tree,yes_detect_RF,yes_detect_boosting)+0.05), main = "Yes purchase detection", xlab = "Número de iteración", ylab = "Quantity of yes purchased detection")
points(yes_detect_knn, col = "blue", type = "b")
points(yes_detect_bayes, col = "red", type = "b")
points(yes_detect_tree, col = "lightblue3", type = "b")
points(yes_detect_RF, col = "olivedrab", type = "b")
points(yes_detect_boosting, col = "orange3", type = "b")
#points(yes_detect_red, col = "rosybrown4", type = "b")
legend("topright", legend = c("SVM","KNN","Bayes","Árbol","RF","Potenciación")
       , col = c("magenta", "blue","red","lightblue3","olivedrab","orange3"), lty = 1, lwd = 1)



plot(error_detect_svm, col = "magenta", type = "b",  ylim = c(min(error_detect_svm,error_detect_knn,error_detect_bayes,error_detect_tree,error_detect_RF,error_detect_boosting), max(error_detect_svm,error_detect_knn,error_detect_bayes,error_detect_tree,error_detect_RF,error_detect_boosting)+3), main = "Detección del ERROR", xlab = "Número de iteración", ylab = "ERROR Cometido")
points(error_detect_knn, col = "blue", type = "b")
points(error_detect_bayes, col = "red", type = "b")
points(error_detect_tree, col = "lightblue3", type = "b")
points(error_detect_RF, col = "olivedrab", type = "b")
points(error_detect_boosting, col = "orange3", type = "b")
legend("topright", legend = c("SVM","KNN","Bayes","Árbol","RF","Potenciación"), col = c("magenta", "blue","red","lightblue3","olivedrab","orange3"), lty = 1, lwd = 2)