#Laboratorio N°3

#Autores:
#   - Nicolás Alarcón L.
#   - Pedro Silva A.

#Se cargan packages
#library(arulesViz)

library("neuralnet")
library(MASS)
library(caret)


workdir_path <- "~/code/github.com/HectorPerezM/lab_ic_2020/lab_3"
setwd(workdir_path)

# Dataset path
dataset_path <- "~/Desktop/Inteligencia/lab_ic_2020/dataset/mushroom/agaricus-lepiota.data"

# Load dataset
data <- read.csv(dataset_path, header = FALSE)

names(data) <- c("type", "cap_shape", "cap_surface", "cap_color", "has_bruises", "odor", "gill_attachment", "gill_spacing", "gill_size", "gill_color", "stalk_shape", "stalk_root", "stalk_surface_above_ring", "stalk_surface_below_ring", "stalk_color_above_ring", "stalk_color_below_ring", "veil_type", "veil_color", "ring_number", "ring_type", "spore_print_color", "population", "habitat")


levels(data$type) <- c('edible', 'poisonous')
levels(data$cap_shape) <- c('bell', 'conical', 'flat', 'knobbed', 'sunken', 'convex')
levels(data$cap_surface) <- c('fibrous', 'grooves', 'scaly', 'smooth')
levels(data$cap_color) <- c('buff', 'cinnamon', 'red', 'gray', 'brown', 'pink', 'green', 'purple', 'white', 'yellow')
levels(data$has_bruises) <- c('bruises', 'no_bruises')
levels(data$odor) <- c('almond', 'creosote', 'foul', 'anise', 'musty', 'none', 'pungent', 'spicy', 'fishy')
levels(data$gill_attachment) <- c('attached', 'free')
levels(data$gill_spacing) <- c('close', 'crowded')
levels(data$gill_size) <- c('broad', 'narrow')
levels(data$gill_color) <- c('buff', 'red', 'gray', 'chocolate', 'black', 'brown', 'orange', 'pink', 'green', 'purple', 'white', 'yellow')
levels(data$stalk_shape) <- c('enlarging', 'tapering')
levels(data$stalk_root) <- c('missing', 'bulbous', 'club', 'equal', 'rooted')
levels(data$stalk_surface_above_ring) <- c('fibrous', 'silky', 'smooth', 'scaly')
levels(data$stalk_surface_below_ring) <- c('fibrous', 'silky', 'smooth', 'scaly')
levels(data$stalk_color_above_ring) <- c('buff', 'cinnamon', 'red', 'gray', 'brown', 'orange', 'pink', 'white', 'yellow')
levels(data$stalk_color_below_ring) <- c('buff', 'cinnamon', 'red', 'gray', 'brown', 'orange', 'pink', 'white', 'yellow')
levels(data$veil_color) <- c('brown', 'orange', 'white', 'yellow')
levels(data$ring_number) <- c('none', 'one', 'two')
levels(data$ring_type) <- c('evanescent', 'flaring', 'large', 'none', 'pendant')
levels(data$spore_print_color) <- c('buff', 'chocolate', 'black', 'brown', 'orange', 'green', 'purple', 'white', 'yellow')
levels(data$population) <- c('abundant', 'clustered', 'numerous', 'scattered', 'several', 'solitary')
levels(data$habitat) <- c('woods', 'grasses', 'leaves', 'meadows', 'paths', 'urban', 'waste')


#Pre-processing
#Remove stalk_root = "missing"
data<-data[!(data$stalk_root == "missing"),]

#Eliminate veil_type
data <- data[,-17]

## TECNICA DE FLATERING 
dummy <- dummyVars("~ .", data = data)
#dummy_clean <- dummyVars("~ .", data = data_clean)
data_ohe <- data.frame(predict(dummy, newdata = data))


## Dividimos el conjunto en datos de entrenamientos y set prueba (75 y 25)
set.seed(500)
index <- sample(1:nrow(data_ohe),round(0.75*nrow(data)))
train <- data_ohe[index,]
test <- data_ohe[-index,]

## Realizamos una regresión legistica :
lm.fit <- glm(typee~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)

#Dado que estamos tratando con un problema de regresión, usaremos el error cuadrático medio (MSE) 
#como una medida de cuánto nuestras predicciones están lejos de los datos reales.
MSE.lm <- sum((pr.lm - test$typee)^2)/nrow(test)

##Preparándose para adaptarse a la red neuronal##
#primero  debemos normalizar los datos antes de entrenar la red neuronal
#maxs <- apply(data_ohe, 2, max) 
#mins <- apply(data_ohe, 2, min)
#scaled <- as.data.frame(scale(data_ohe, center = mins, scale = maxs - mins))
#train_ <- scaled[index,]
#test_ <- scaled[-index,]

#Parámetros# 
library(neuralnet)
n <- names(train)
f <- as.formula(paste("typee ~", paste(n[!n %in% "typee"], collapse = " + ")))
nn <- neuralnet(f,data=train,hidden=c(5,3),linear.output=T)
plot(nn)

pr.nn <- compute(nn,test[,1:118])
pr.nn_ <- pr.nn$net.result*(max(data_ohe$typee)-min(data_ohe$typee))+min(data_ohe$typee)
test.r <- (test$typee)*(max(data_ohe$typee)-min(data_ohe$typee))+min(data_ohe$typee)
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(test)


print(paste(MSE.lm,MSE.nn))
par(mfrow=c(1,2))
plot(test$typee,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')
plot(test$typee,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)

plot(test$typee,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
points(test$typee,pr.lm,col='blue',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))

library(boot)
set.seed(200)
lm.fit <- glm(typee~.,data=data_ohe)
cv.glm(data_ohe,lm.fit,K=10)$delta[1]


set.seed(450)
cv.error <- NULL
k <- 10
library(plyr) 
pbar <- create_progress_bar('text')
pbar$init(k)
for(i in 1:k){
  index <- sample(1:nrow(data_ohe),round(0.9*nrow(data_ohe)))
  train.cv <- scaled[index,]
  test.cv <- scaled[-index,]
  nn <- neuralnet(f,data=train.cv,hidden=c(5,2),linear.output=T)   
  pr.nn <- compute(nn,test.cv[,1:118])
  pr.nn <- pr.nn$net.result*(max(data_ohe$typee)-min(data_ohe$typee))+min(data_ohe$typee)   
  test.cv.r <- (test.cv$typee)*(max(data_ohe$typee)-min(data_ohe$typee))+min(data_ohe$typee)   
  cv.error[i] <- sum((test.cv.r - pr.nn)^2)/nrow(test.cv)    
  pbar$step()
}

mean(cv.error)
cv.error

boxplot(cv.error,xlab='MSE CV',col='cyan',
        border='blue',names='CV error (MSE)',
        main='CV error (MSE) for NN',horizontal=TRUE)


