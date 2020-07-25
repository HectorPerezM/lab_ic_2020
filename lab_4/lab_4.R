#Laboratorio N°4

#Autores:
#   - Nicolás Alarcón L.
#   - Pedro Silva A.
#   - Héctor Pérez M.


#libraries
library(C50) # create decision tree model 
library(caTools) # for split dataset
library(gmodels)  # for cross table
library(OneR) #for create decision tree model but with OneR

#Cambiar de ser necesario
workdir_path <- "~/code/github.com/HectorPerezM/lab_ic_2020/lab_4"
setwd(workdir_path)

# Dataset path
dataset_path <- "../dataset/mushroom/agaricus-lepiota.data"

# Load dataset
data <- read.csv(dataset_path, header = FALSE)

# Give names to variables
names(data) <- c("type", "cap_shape", "cap_surface", "cap_color", "has_bruises", "odor", "gill_attachment", "gill_spacing", "gill_size", "gill_color", "stalk_shape", "stalk_root", "stalk_surface_above_ring", "stalk_surface_below_ring", "stalk_color_above_ring", "stalk_color_below_ring", "veil_type", "veil_color", "ring_number", "ring_type", "spore_print_color", "population", "habitat")

# ---------- Preprocessing ---------------------

#Remove stalk_root = "missing"
data<-data[!(data$stalk_root == "?"),]

#Eliminate veil_type
data$veil_type <- NULL

set.seed(123)

#From: https://stackoverflow.com/questions/17200114/how-to-split-data-into-training-testing-sets-using-sample-function
#Separated dataset into train (80%) and test (20%)
sample <- sample.split(data[,1], SplitRatio = 0.80)
train_sample <- subset(data, sample == TRUE)
test_sample <- subset(data, sample == FALSE)

# ---------- End -------------------------------

# ---------- Decision Tree using C50 package ---------------------
# From: https://github.com/neburs/mushrooms-classification

c50_model <- C5.0(train_sample[,-1], train_sample$type, rules = FALSE)
summary(c50_model)


c50_model_prediction <- predict(c50_model, test_sample, type="class")
CrossTable(test_sample$type, c50_model_prediction, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('Actual Default', 'Predicted Default'))

#Accuracy
sum(c50_model_prediction == test_sample$type) / length(c50_model_prediction)

#Plot
plot(c50_model)
# ---------- End Decision Tree using C50 package -----------------

# ---------- Decision Tree using OneR package --------------------
# From:  Machine Learning with R: Expert techniques for predictive modeling, 3rd Edition. Author: Brett Lantz
#        page: 158 - 162

oneR_model <- OneR(type ~ ., data = data)
oneR_model

oneR_model_prediction <- predict(oneR_model, data)
table(actual = data$type, predicted = oneR_model_prediction)
# ---------- End Decision Tree using OneR package ----------------
