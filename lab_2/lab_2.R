  # Laboratorio N°2
  
  # librerias
  library(ggpubr)
  library(dplyr)
  library(rockchalk)
  
  #Libreria requerida para realizar la 2da experiencia
  library(cluster)
  
  # clustering algorithms & visualization
  library(factoextra)
  
  # Para realizar 'one-hot encoding'
  library(caret)
  
  
  # Set working directory
  # change as needed
  workdir_path <- "~/code/github.com/HectorPerezM/lab_ic_2020/lab_2"
  setwd(workdir_path)
  # Dataset path
    dataset_path <- "~/Desktop/Inteligencia/lab_ic_2020/dataset/mushroom/agaricus-lepiota.data"
  # Load dataset
  data <- read.csv(dataset_path, header = FALSE)
  
  # Rename columns for better comprehension
  names(data) <- c("type", "cap_shape", "cap_surface", "cap_color", "has_bruises", "odor", "gill_attachment", "gill_spacing", "gill_size", "gill_color", "stalk_shape", "stalk_root", "stalk_surface_above_ring", "stalk_surface_below_ring", "stalk_color_above_ring", "stalk_color_below_ring", "veil_type", "veil_color", "ring_number", "ring_type", "spore_print_color", "population", "habitat")
  
  #Objetivo:
  # Extraer el conocimiento del problema asignado, obteniendo mediante el software R, utilizando el
  # algoritmo de clustering K-means y realizar el análisis respectivo. Luego debe comparar con los
  # resultados revisados en la literatura encontrada y ver si se sustenta el conocimiento obtenido.
  # Para ello se debe identificar el número de grupos a generar (en base a un criterio que usted  
  # defina). Luego se debe realizar el análisis (centroides) e identificar qué características podrían
  # estar asociadas a una u otra clase.
  
  
  # Pre-procesamiento
  
  # Transformamos las categorias para que sean legibles
  # https://stackoverflow.com/questions/29711067/how-to-change-name-of-factor-levels
  
  
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
  
  
  
  
  # Debido a que 'veil_type' solo posee observaciones de 1 categoría, no nos aporta nada relevante
  # por lo tanto se decide eliminar esa columna. Además, k-mean arroja un error si una variable posee solo1 level
  data<-data[!(data$stalk_root == "missing"),]
  data <- data[,-17]
  
  dummy <- dummyVars("~ .", data = data)
  #dummy_clean <- dummyVars("~ .", data = data_clean)
  data_ohe <- data.frame(predict(dummy, newdata = data))
  
  
  #data_clean <- data[,-17]
  
  
  # Se sabe, de la experiencia anterior, que 'stalk_root' posee una categoría llamada 'missing', por lo que se decide 
  # hacer experimentos de k-mean con 2 dataset, uno que contenga las observaciones de 'missing' y otro que no.
  # Entonces:
  #     data_ro no contiene las observaciones con stalk_root = ?
  #     data contiene las obv. con stalk_root = ?
  #
  # https://stackoverflow.com/questions/52120149/how-to-delete-rows-of-data-within-a-certain-category-in-r
  # https://rpubs.com/m3cinc/Machine_Learning_Classification_Challenges
  # https://uc-r.github.io/kmeans_clustering#prep
  
  #data_clean$stalk_root[data_clean$stalk_root == 'missing'] <- NA
  #data_clean <- na.omit(data_clean)
  #data_clean$stalk_root <- factor(data_clean$stalk_root)
  

  matriz.distancia <- daisy(data_ohe, metric = "gower")
  matrix.similutd<- as.matrix(matriz.distancia)
  # Eleguiendo el numero de grupos #.
  
  
  
  fviz_nbclust(mtrix.similutd,kmeans,method="silhouette")
  # Optimo = 2 clusters 
  fviz_nbclust(matrix.similutd,pam,method="silhouette")
  # Optimo = 2 clusters 
  




# ------------------------------
# Clustering
# Revisar:
#     -> https://www.datanovia.com/en/lessons/k-means-clustering-in-r-algorith-and-practical-examples/
#     -> https://uc-r.github.io/kmeans_clustering
#     ->


#Para que los experimentos sean reproducibles
set.seed(150)

# K-Mean
kmeans.result = kmeans(x = matrix.similutd, centers = 2)
graph.kmeans= fviz_cluster(kmeans.result
                                      , data =matrix.similutd, stand = TRUE,
                                      geom = "point", 
                                      ellipse = TRUE, ellypse.type = "convex")
graph.kmeans
kmeans_table  <- table(data$type, kmeans_result$cluster)
print(kmeans_table)


pam.result  = pam(x = matrix.similutd, k = 2)


graph.pam= fviz_cluster(pam.result
                           , data =matrix.similutd, stand = TRUE,
                           geom = "point", 
                           ellipse = TRUE, ellypse.type = "convex")
graph.pam
pam_table  <- table(data$type, pam.result$cluster)
print(pam_table)

# PAM 
#   -> https://es.wikipedia.org/wiki/K-medoids
#   con F1 puedes ver la def. de la funcion

pam_result <- pam(x = data_ohe, k = 2)


data["cluster.kmeans"] <- kmeans.result$cluster
grupo.kmeans.1 <- summary(data[data$cluster.kmeans ==1,])
grupo.kmeans.2 <- summary(data[data$cluster.kmeans ==2,])


data["cluster.Pam"] <- pam.result$cluster
grupo.pam.1 <- summary(data[data$cluster.pam ==1,])
grupo.pam.2 <- summary(data[data$cluster.pam ==2,])



#gower_dist <- daisy(data_clean_tf, metric = 'gower')
#gower_mat <- as.matrix(gower_dist)
#sil_width <- c(NA)
#for(i in 2:8){  
#  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
#  sil_width[i] <- pam_fit$silinfo$avg.width  
#}
#plot(1:8, sil_width,
#     xlab = "Number of clusters",
#     ylab = "Silhouette Width")
#lines(1:8, sil_width)