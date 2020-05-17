# Laboratorio N°1

# Load ggpubr library for better plots
library(ggpubr)

# Load dplyr to handle classes in columns
library(dplyr)

#To group levels when necesary
library(rockchalk)

# Set working directory
# change as needed
workdir_path <- "~/code/github.com/HectorPerezM/lab_ic_2020/lab_1"
setwd(workdir_path)

# Dataset path
dataset_names_path <- "../dataset/mushroom/agaricus-lepiota.names"
dataset_path <- "../dataset/mushroom/agaricus-lepiota.data"

data <- read.csv(dataset_path, header = FALSE)

# Rename columns for better comprehension
names(data) <- c("type", "cap_shape", "cap_surface", "cap_color", "has_bruises", "odor", "gill_attachment", "gill_spacing", "gill_size", "gill_color", "stalk_shape", "stalk_root", "stalk_surface_above_ring", "stalk_surface_below_ring", "stalk_color_above_ring", "stalk_color_below_ring", "veil_type", "veil_color", "ring_number", "ring_type", "spore_print_color", "population", "habitat")

# ---------- Cleaning data -------------------------

# Eliminate cap_surface -> grooves obs
data <- droplevels(data[-which(data$cap_surface == "g"),])

# In var cap_color
#Regroup categories: "b", "c", "r", "p", "u"
data$cap_color <- as.character(data$cap_color)
data$cap_color <- factor( with(data, replace(cap_color, cap_color %in% c( "b", "c", "r", "p", "u"), "o")))

#data$stalk_root <- as.character(data$stalk_root)
#data$stalk_root <- factor( with(data, replace(stalk_color, stalk_color %in% c( "b", "c", "r", "p", "u"), "o")))

# In var stalk_surface_above_ring
# regroup in others (o) categories fibrous (f) and scaly (y)
data$stalk_surface_above_ring <- as.character(data$stalk_surface_above_ring)
data$stalk_surface_above_ring <- factor( with(data, replace(stalk_surface_above_ring, stalk_surface_above_ring %in% c( "f", "y"), "o")))

# In var stalk_surface_below_ring
# regroup in others (o) categories fibrous (f) and scaly (y)
data$stalk_surface_below_ring <- as.character(data$stalk_surface_below_ring)
data$stalk_surface_below_ring <- factor( with(data, replace(stalk_surface_below_ring, stalk_surface_below_ring %in% c( "f", "y"), "o")))

#-----------------------------------------------------



# Chi2 Test on variables
# Necesitamos conocer que variables se relacionan con nuestra variable a predecir, que en este caso es si es comestible o no (data$type)
# para ello realizamos un test de chi2 por cada par de variables, es decir, en cada uno testeamos la hipotesis nula que sería:

# H0: La variable data$type y la variable data$<x_variables> son independientes 
# H1: La variable data$type y la variable data$<x_variables> son dependientes

# Con un p-value = 0.05, si es menor se rechaza la hipotesis nula y se acepta la hipotesis 1

#Funcion que realiza un test de chi2 a todas las variables de dataset
#filtra la variable veil_type debido a que como presenta 1 solo nivel, arroja un error al momento de realizar el test
chisqTestToDataset <- function(toTestName, toTest) {
  for(i in 1:length(names(data))) {
    #Filtro veil_type debido a que solo posee 1 level
    if(names(data[i]) != "veil_type") {
      print("---------------- \n")
      cat("Chi2 Test of ", toTestName, " and ", names(data[i]))
      print(chisq.test(toTest, data[[i]]))
    }
  }
}

#Llamamos a la funcion, le entregamos los parametros
chisqTestToDataset("Type", data$type)

# Excel con resultados ordenados: https://docs.google.com/spreadsheets/d/16Ku-c6ySvrFHFqivhdIiomCuOsa_ZYPzJOHVYyqro1I/edit?usp=sharing

# Cramer test

# Regresion Logistica
logit <- glm(formula = type ~ cap_shape + cap_surface + cap_color + has_bruises + 
                              odor + gill_attachment + gill_spacing + gill_size + 
                              gill_color + stalk_shape + stalk_root + stalk_surface_above_ring +
                              stalk_surface_below_ring + stalk_color_above_ring + stalk_color_below_ring +
                              veil_color + ring_number + ring_type + spore_print_color + 
                              population + habitat, data = data, family = "binomial")
summary(logit)