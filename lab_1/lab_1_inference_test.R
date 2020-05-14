# Laboratorio N°1

# Load ggpubr library for better plots
library(ggpubr)

# Load dplyr to handle classes in columns
library(dplyr)

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



# Chi2 Test on variables

#REVISAR RAZONAMIENTO, PUEDE ESTAR MALO !!!

# Necesitamos conocer que variables se relacionan con nuestra variable a predecir, que en este caso es si es comestible o no (data$type)
# para ello realizamos un test de chi2 por cada par de variables, es decir, en cada uno testeamos la hipotesis nula que sería:

# H0: La variable data$type y la variable data$<resto de variables> son independientes 
# H1: La variable data$type y la variable data$<resto de variables> son dependientes

# Es importante conocer que variables son dependientes con nuestra variable a predecir, así sabemos realmente
# cuales de ellas influencian a los algoritmos que implementaremos a futuro.

column_names <- c("type", "cap_shape", "cap_surface", "cap_color", "has_bruises")
x <- column_names[1]
print(data$x)
numberColumns <- length(column_names)

test <- function(names) {
  for (i in 1:length(names)) {
    print(data[names[i]])
  }
}

test(column_names)

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

chisqTestToDataset("Type", data$type)

## ANOTAR SALIDA EN EXCEL Y ORDENAR!!!!

cat(data$type)
print(length(names(data)))
print(names(data[2]))

x <- chisq.test(data$type, data$cap_surface)

print(x$statistic[1])

