#Laboratorio N°4

#Autores:
#   - Nicolás Alarcón L.
#   - Pedro Silva A.
#   - Héctor Pérez M.


#libraries
library(C50) # create decision tree model 
library(caTools) # for split dataset
library(gmodels)  # for cross table

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

# ---------- Decision Tree using C50 package Case 1 ---------------------
# From: https://github.com/neburs/mushrooms-classification
c50_model <- C5.0(train_sample[,-1], train_sample$type, 
                  control = C5.0Control(winnow = FALSE, noGlobalPruning = TRUE, earlyStopping = TRUE, CF = 0.25),
                  rules = FALSE)

# Summary
summary(c50_model)

# Var importance
C5imp(c50_model, metric = "usage", pct = TRUE)


# Pred
c50_model_prediction <- predict(c50_model, test_sample, type="class")
CrossTable(test_sample$type, c50_model_prediction, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('Actual Default', 'Predicted Default'))

#Accuracy
sum(c50_model_prediction == test_sample$type) / length(c50_model_prediction)

#Plot
plot(c50_model)
# ---------- End Decision Tree using C50 package -------------------------

# ---------- Decision Tree using C50 package Case 2 ---------------------
# Se varia CF a 0.7, winnow a TRUE, noGlobalPrunning FALSE, earlyStopping a FALSE
c50_model_modified <- C5.0(train_sample[,-1], train_sample$type, 
                  control = C5.0Control(winnow = TRUE, noGlobalPruning = FALSE, earlyStopping = FALSE, CF = 0.7),
                  rules = FALSE)

# Summary
summary(c50_model_modified)

# Var importance
C5imp(c50_model_modified, metric = "usage", pct = TRUE)


# Pred
c50_model_modified_prediction <- predict(c50_model_modified, test_sample, type="class")
CrossTable(test_sample$type, c50_model_modified_prediction, prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, dnn = c('Actual Default', 'Predicted Default'))

#Accuracy
sum(c50_model_modified_prediction == test_sample$type) / length(c50_model_modified_prediction)

#Plot
plot(c50_model_modified)
# ---------- End Decision Tree using C50 package -------------------------
