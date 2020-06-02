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
  
  
  ## descomentar ## 
  #fviz_nbclust(matrix.similutd,kmeans,method="silhouette")
  # Optimo = 2 clusters 
  #fviz_nbclust(matrix.similutd,pam,method="silhouette")
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
kmeans_table  <- table(data$type, kmeans.result$cluster)
print(kmeans_table)


#pam.result  = pam(x = matrix.similutd, k = 2)


#graph.pam= fviz_cluster(pam.result
#                           , data =matrix.similutd, stand = TRUE,
#                           geom = "point", 
#                           ellipse = TRUE, ellypse.type = "convex")
#graph.pam
#pam_table  <- table(data$type, pam.result$cluster)
#print(pam_table)

# PAM 
#   -> https://es.wikipedia.org/wiki/K-medoids
#   con F1 puedes ver la def. de la funcion

#pam_result <- pam(x = data_ohe, k = 2)


data["cluster.kmeans"] <- kmeans.result$cluster
grupo.kmeans.1 <- summary(data[data$cluster.kmeans ==1,])
grupo.kmeans.2 <- summary(data[data$cluster.kmeans ==2,])



grupo1.type <-ggplot(data=data[data$cluster.kmeans ==1,], aes(x=type,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs Type")
grupo1.type

grupo1.cap_shape<- ggplot(data=data[data$cluster.kmeans ==1,], aes(x=cap_shape,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs cap_shape")
grupo1.cap_shape

grupo1.cap_surface<- ggplot(data=data[data$cluster.kmeans ==1,], aes(x=cap_surface,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs cap_surface")
grupo1.cap_surface

grupo1.cap_color<- ggplot(data=data[data$cluster.kmeans ==1,], aes(x=cap_color,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs cap_color")
grupo1.cap_color

grupo1.has_bruises<- ggplot(data=data[data$cluster.kmeans ==1,], aes(x=has_bruises,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs has_bruises")
grupo1.has_bruises


grupo1.odor<- ggplot(data=data[data$cluster.kmeans ==1,], aes(x=odor,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs odor")
grupo1.odor

grupo1.gill_attachment<- ggplot(data=data[data$cluster.kmeans ==1,], aes(x=gill_attachment,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs gill_attachment")
grupo1.gill_attachment

gripo1.gill_spacing<- ggplot(data=data[data$cluster.kmeans ==1,], aes(x=gill_spacing,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs gill_spacing")
gripo1.gill_spacing

grupo1.gill_size <- ggplot(data=data[data$cluster.kmeans ==1,], aes(x=gill_size,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs gill_size")
grupo1.gill_size

grupo1.gill_color <- ggplot(data=data[data$cluster.kmeans ==1,], aes(x=gill_color,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs gill_color")
grupo1.gill_color

grupo1.stalk_shape<- ggplot(data=data[data$cluster.kmeans ==1,], aes(x=stalk_shape,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs stalk_shape")
grupo1.stalk_shape


grupo1.stalk_root<- ggplot(data=data[data$cluster.kmeans ==1,], aes(x=stalk_root,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs stalk_root")
grupo1.stalk_root


grupo1.stalk_surface_above_ring<- ggplot(data=data[data$cluster.kmeans ==1,], aes(x=stalk_surface_above_ring,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs stalk_surface_above_ring")
grupo1.stalk_surface_above_ring

grupo1.stalk_surface_below_ring<- ggplot(data=data[data$cluster.kmeans ==1,], aes(x=stalk_surface_below_ring,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs stalk_surface_below_ring")
grupo1.stalk_surface_below_ring


grupo1.stalk_color_above_ring<- ggplot(data=data[data$cluster.kmeans ==1,], aes(x=stalk_color_above_ring,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs stalk_color_above_ring")
grupo1.stalk_color_above_ring

grupo1.stalk_color_below_ring<- ggplot(data=data[data$cluster.kmeans ==1,], aes(x=stalk_color_below_ring,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs stalk_color_below_ring")
grupo1.stalk_color_below_ring


grupo1.veil_color<- ggplot(data=data[data$cluster.kmeans ==1,], aes(x=veil_color,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs veil_color")
grupo1.veil_color

grupo1.ring_number<- ggplot(data=data[data$cluster.kmeans ==1,], aes(x=ring_number,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs ring_number")
grupo1.ring_number

grupo1.ring_type<- ggplot(data=data[data$cluster.kmeans ==1,], aes(x=ring_type,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs ring_type")
grupo1.ring_type

grupo1.spore_print_color<- ggplot(data=data[data$cluster.kmeans ==1,], aes(x=spore_print_color,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs spore_print_color")
grupo1.spore_print_color


grupo1.population<- ggplot(data=data[data$cluster.kmeans ==1,], aes(x=population,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs population")
grupo1.population

grupo1.habitat<- ggplot(data=data[data$cluster.kmeans ==1,], aes(x=habitat,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs habitat")
grupo1.habitat



### GRUPO 2 #

grupo2.type <-ggplot(data=data[data$cluster.kmeans ==2,], aes(x=type,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs type ")
grupo2.type

grupo2.cap_shape<- ggplot(data=data[data$cluster.kmeans ==2,], aes(x=cap_shape,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs cap_shape")
grupo2.cap_shape

grupo2.cap_surface<- ggplot(data=data[data$cluster.kmeans ==2,], aes(x=cap_surface,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs cap_surface ")
grupo2.cap_surface

grupo2.cap_color<- ggplot(data=data[data$cluster.kmeans ==2,], aes(x=cap_color,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs cap_color")
grupo2.cap_color

grupo2.has_bruises<- ggplot(data=data[data$cluster.kmeans ==2,], aes(x=has_bruises,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs has_bruises")
grupo2.has_bruises


grupo2.odor<- ggplot(data=data[data$cluster.kmeans ==2,], aes(x=odor,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs odor")
grupo2.odor

grupo2.gill_attachment<- ggplot(data=data[data$cluster.kmeans ==2,], aes(x=gill_attachment,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs gill_attachment")
grupo2.gill_attachment

gripo2.gill_spacing<- ggplot(data=data[data$cluster.kmeans ==2,], aes(x=gill_spacing,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs gill_spacing")
gripo2.gill_spacing

grupo2.gill_size <- ggplot(data=data[data$cluster.kmeans ==2,], aes(x=gill_size,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs gill_size")
grupo2.gill_size

grupo2.gill_color <- ggplot(data=data[data$cluster.kmeans ==2,], aes(x=gill_color,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs gill_color")
grupo2.gill_color

grupo2.stalk_shape<- ggplot(data=data[data$cluster.kmeans ==2,], aes(x=stalk_shape,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs stalk_shape")
grupo2.stalk_shape


grupo2.stalk_root<- ggplot(data=data[data$cluster.kmeans ==2,], aes(x=stalk_root,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs stalk_root")
grupo2.stalk_root


grupo2.stalk_surface_above_ring<- ggplot(data=data[data$cluster.kmeans ==2,], aes(x=stalk_surface_above_ring,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs stalk_surface_above_ring")
grupo2.stalk_surface_above_ring

grupo2.stalk_surface_below_ring<- ggplot(data=data[data$cluster.kmeans ==2,], aes(x=stalk_surface_below_ring,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs stalk_surface_below_ring")
grupo2.stalk_surface_below_ring


grupo2.stalk_color_above_ring<- ggplot(data=data[data$cluster.kmeans ==2,], aes(x=stalk_color_above_ring,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs stalk_color_above_ring")
grupo2.stalk_color_above_ring

grupo2.stalk_color_below_ring<- ggplot(data=data[data$cluster.kmeans ==2,], aes(x=stalk_color_below_ring,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs stalk_color_below_ring")
grupo2.stalk_color_below_ring


grupo2.veil_color<- ggplot(data=data[data$cluster.kmeans ==2,], aes(x=veil_color,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs veil_color")
grupo2.veil_color

grupo2.ring_number<- ggplot(data=data[data$cluster.kmeans ==2,], aes(x=ring_number,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs ring_number")
grupo2.ring_number

grupo2.ring_type<- ggplot(data=data[data$cluster.kmeans ==2,], aes(x=ring_type,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs ring_type")
grupo2.ring_type

grupo2.spore_print_color<- ggplot(data=data[data$cluster.kmeans ==2,], aes(x=spore_print_color,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs spore_print_color")
grupo2.spore_print_color


grupo2.population<- ggplot(data=data[data$cluster.kmeans ==2,], aes(x=population,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs population")
grupo2.population

grupo2.habitat<- ggplot(data=data[data$cluster.kmeans ==2,], aes(x=habitat,y = ..prop..,group= 1)) + geom_bar(stat="count") +ggtitle("Prop vs habitat")
grupo2.habitat



