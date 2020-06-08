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
dataset_path <- "../dataset/mushroom/agaricus-lepiota.data"
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
data_ohe <- data.frame(predict(dummy, newdata = data))


# Matriz Simulitud usando Gower
matriz.distancia <- daisy(data_ohe, metric = "gower")
matrix.similutd<- as.matrix(matriz.distancia)

# Descomentar solo si se desea calcular el numero
# optimo de grupos
# Advertencia: Demora mucho tiempo

#K-Means
#fviz_nbclust(matrix.similutd,kmeans,method="silhouette")
# Resultado = 2 clusters 

#PAM
#fviz_nbclust(matrix.similutd,pam,method="silhouette")
# Resultado = 2 clusters 

# ------------------------------
# Clustering
#-------------------------------

#Para que los experimentos sean reproducibles
set.seed(150)

#K-Mean

kmeans.result = kmeans(x = matrix.similutd, centers = 2)
kmeans.3.result = kmeans(x = matrix.similutd, centers = 3)
kmeans.4.result = kmeans(x = matrix.similutd, centers = 4)

graph.kmeans= fviz_cluster(kmeans.result
                           , data =matrix.similutd, stand = TRUE,
                           geom = "point", 
                           ellipse = TRUE, ellypse.type = "convex")

graph.kmeans.3= fviz_cluster(kmeans.3.result
                           , data =matrix.similutd, stand = TRUE,
                           geom = "point", 
                           ellipse = TRUE, ellypse.type = "convex")

graph.kmeans.4 = fviz_cluster(kmeans.4.result
                           , data =matrix.similutd, stand = TRUE,
                           geom = "point", 
                           ellipse = TRUE, ellypse.type = "convex")

#Imprime graficos
graph.kmeans
graph.kmeans.3
graph.kmeans.4

#Tablas
kmeans_table  <- table(data$type, kmeans.result$cluster)
kmeans_table_result3  <- table(data$type, kmeans.3.result$cluster)
kmeans_table_result4  <- table(data$type, kmeans.4.result$cluster)

#Separo los grupos para k-means = 2
data["cluster.kmeans"] <- kmeans.result$cluster
grupo.kmeans.1 <- summary(data[data$cluster.kmeans == 1,])
grupo.kmeans.2 <- summary(data[data$cluster.kmeans == 2,])


#PAM

pam.result  = pam(x = matrix.similutd, k = 2)
graph.pam= fviz_cluster(pam.result, data =matrix.similutd, stand = TRUE, geom = "point", ellipse = TRUE, ellypse.type = "convex")

graph.pam
pam_table  <- table(data$type, pam.result$cluster)
print(pam_table)

#--------------------------------
# Gráficos
#--------------------------------

# type -------------------
type_group_1 <- prop.table(100*table(data[data$cluster.kmeans ==1,]$type))
type_group_2 <- prop.table(100*table(data[data$cluster.kmeans ==2,]$type))
type_df <- data.frame(type=c("edible", "poisonous"),
                      n_mushrooms=c(type_group_1, type_group_2),
                      group=rep(c("1", "2"), each = 2))
ggbarplot(type_df, x = "type", y = "n_mushrooms", xlab=c("type"), ylab="%",
          fill="group", color="group", position = position_dodge(0.8), lab.col = "group",
          palette = c("#006266", "#C4E538"),
          title = "type Proportion", label = TRUE, label.pos = "out", lab.nb.digits = 2)


#Kmeans 3 Grupos
data["cluster.kmeans"] <- kmeans.3.result$cluster
kmeans_3_table  <- table(data$type, kmeans.3.result$cluster)
print(kmeans_3_table)

grupo.3.kmeans.1 <- summary(data[data$cluster.kmeans == 1,])
grupo.3.kmeans.2 <- summary(data[data$cluster.kmeans == 2,])
grupo.3.kmeans.3 <- summary(data[data$cluster.kmeans == 3,])

type_group_1 <- prop.table(100*table(data[data$cluster.kmeans ==1,]$type))
type_group_2 <- prop.table(100*table(data[data$cluster.kmeans ==2,]$type))
type_group_3 <- prop.table(100*table(data[data$cluster.kmeans ==3,]$type))
print(table(data[data$cluster.kmeans]$type))

type_df <- data.frame(type=c("edible", "poisonous"),
                      n_mushrooms=c(type_group_1, type_group_2, type_group_3),
                      group=rep(c("1", "2", "3"), each = 2))

ggbarplot(type_df, x = "type", y = "n_mushrooms", xlab=c("type"), ylab="%",
          fill="group", color="group", position = position_dodge(0.8), lab.col = "group",
          palette = c("#006266", "#C4E538", "#e67e22"),
          title = "type Proportion", label = TRUE, label.pos = "out", lab.nb.digits = 2)


#Kmeans 4 Grupos
  data["cluster.kmeans"] <- kmeans.4.result$cluster
  kmeans_4_table  <- table(data$type, kmeans.4.result$cluster)
  print(kmeans_4_table)


grupo.4.kmeans.1 <- summary(data[data$cluster.kmeans == 1,])
grupo.4.kmeans.2 <- summary(data[data$cluster.kmeans == 2,])
grupo.4.kmeans.3 <- summary(data[data$cluster.kmeans == 3,])
grupo.4.kmeans.4 <- summary(data[data$cluster.kmeans == 4,])


type_group_1 <- prop.table(100*table(data[data$cluster.kmeans ==1,]$type))
type_group_2 <- prop.table(100*table(data[data$cluster.kmeans ==2,]$type))
type_group_3 <- prop.table(100*table(data[data$cluster.kmeans ==3,]$type))
type_group_4 <- prop.table(100*table(data[data$cluster.kmeans ==4,]$type))

type_df <- data.frame(type=c("edible", "poisonous"),
                      n_mushrooms=c(type_group_1, type_group_2, type_group_3, type_group_4),
                      group=rep(c("1", "2", "3", "4"), each = 2))

ggbarplot(type_df, x = "type", y = "n_mushrooms", xlab=c("type"), ylab="%",
          fill="group", color="group", position = position_dodge(0.8), lab.col = "group",
          palette = c("#006266", "#C4E538", "#e67e22", "#8e44ad"),
          title = "type Proportion", label = TRUE, label.pos = "out", lab.nb.digits = 2)


# Repetir para volver con los datos de k=2
data["cluster.kmeans"] <- kmeans.result$cluster
grupo.kmeans.1 <- summary(data[data$cluster.kmeans == 1,])
grupo.kmeans.2 <- summary(data[data$cluster.kmeans == 2,])

# cap_shape ---------------
cap_shape_group_1 <- prop.table(100*table(data[data$cluster.kmeans ==1,]$cap_shape))
cap_shape_group_2 <- prop.table(100*table(data[data$cluster.kmeans ==2,]$cap_shape))
cap_shape_df <- data.frame(cap_shape=c("bell", "conical", "flat", "knobbed", "sunken", "convex"),
                           n_mushrooms=c(cap_shape_group_1, cap_shape_group_2),
                           group=rep(c("1", "2"), each = 6))
ggbarplot(cap_shape_df, x = "cap_shape", y = "n_mushrooms", xlab=c("cap_shape"), ylab="%",
          fill="group", color="group", position = position_dodge(0.8), lab.col = "group",
          palette = c("#006266", "#C4E538"),
          title = "cap_shape Proportion", label = TRUE, label.pos = "out", lab.nb.digits = 2)


# cap_surface ---------------------
cap_surface_group_1 <- prop.table(100*table(data[data$cluster.kmeans ==1,]$cap_surface))
cap_surface_group_2 <- prop.table(100*table(data[data$cluster.kmeans ==2,]$cap_surface))
print(cap_surface_group_1)
cap_surface_df <- data.frame(cap_surface=c("fibrous", "grooves", "scaly", "smooth"),
                             n_mushrooms=c(cap_surface_group_1, cap_surface_group_2),
                             group=rep(c("1", "2"), each = 4))
ggbarplot(cap_surface_df, x = "cap_surface", y = "n_mushrooms", xlab=c("cap_surface"), ylab="%",
          fill="group", color="group", position = position_dodge(0.8), lab.col = "group",
          palette = c("#006266", "#C4E538"),
          title = "cap_surface Proportion", label = TRUE, label.pos = "out", lab.nb.digits = 2)

# cap_color ----------------------
cap_color_group_1 <- prop.table(100*table(data[data$cluster.kmeans ==1,]$cap_color))
cap_color_group_2 <- prop.table(100*table(data[data$cluster.kmeans ==2,]$cap_color))
print(cap_color_group_1)
cap_color_df <- data.frame(cap_color=c("buff", "cinnamon", "red", "gray", "brown", "pink", "green", "purple", "white", "yellow"),
                           n_mushrooms=c(cap_color_group_1, cap_color_group_2),
                           group=rep(c("1", "2"), each = 10))
ggbarplot(cap_color_df, x = "cap_color", y = "n_mushrooms", xlab=c("cap_color"), ylab="%",
          fill="group", color="group", position = position_dodge(0.8), lab.col = "group",
          palette = c("#006266", "#C4E538"),
          title = "cap_color Proportion", label = TRUE, label.pos = "out", lab.nb.digits = 2)

# has_bruises ----------------
has_bruises_group_1 <- prop.table(100*table(data[data$cluster.kmeans ==1,]$has_bruises))
has_bruises_group_2 <- prop.table(100*table(data[data$cluster.kmeans ==2,]$has_bruises))
print(has_bruises_group_1)
print(has_bruises_group_2)
has_bruises_df <- data.frame(bruises=c("bruises", "np_bruises"),
                             n_mushrooms=c(has_bruises_group_1, has_bruises_group_2),
                             group=rep(c("1", "2"), each = 2))
ggbarplot(has_bruises_df, x = "bruises", y = "n_mushrooms", xlab=c("bruises"), ylab="%",
          fill="group", color="group", position = position_dodge(0.8), lab.col = "group",
          palette = c("#006266", "#C4E538"),
          title = "bruises Proportion", label = TRUE, label.pos = "out", lab.nb.digits = 2)

# odo -------------------
odor_group_1 <- prop.table(100*table(data[data$cluster.kmeans ==1,]$odor))
odor_group_2 <- prop.table(100*table(data[data$cluster.kmeans ==2,]$odor))
print(odor_group_1)
print(odor_group_2)
odor_df <- data.frame(odor=c("almond", "creosote", "foul", "anise", "musty", "none", "pungent", "spicy", "fishy"),
                      n_mushrooms=c(odor_group_1, odor_group_2),
                      group=rep(c("1", "2"), each = 9))
ggbarplot(odor_df, x = "odor", y = "n_mushrooms", xlab=c("odor"), ylab="%",
          fill="group", color="group", position = position_dodge(0.8), lab.col = "group",
          palette = c("#006266", "#C4E538"),
          title = "odor Proportion", label = TRUE, label.pos = "out", lab.nb.digits = 2)

# gill_attachment --------------------
gill_attachment_group_1 <- prop.table(100*table(data[data$cluster.kmeans ==1,]$gill_attachment))
gill_attachment_group_2 <- prop.table(100*table(data[data$cluster.kmeans ==2,]$gill_attachment))
print(gill_attachment_group_1)
print(gill_attachment_group_2)
gill_attachment_df <- data.frame(x=c("attached", "free"),
                                 n_mushrooms=c(gill_attachment_group_1, gill_attachment_group_2),
                                 group=rep(c("1", "2"), each = 2))
ggbarplot(gill_attachment_df, x = "x", y = "n_mushrooms", xlab=c("gill_attachment"), ylab="%",
          fill="group", color="group", position = position_dodge(0.8), lab.col = "group",
          palette = c("#006266", "#C4E538"),
          title = "gill_attachment Proportion", label = TRUE, label.pos = "out", lab.nb.digits = 2)


# gill_spacing ----------------------
gill_spacing_group_1 <- prop.table(100*table(data[data$cluster.kmeans ==1,]$gill_spacing))
gill_spacing_group_2 <- prop.table(100*table(data[data$cluster.kmeans ==2,]$gill_spacing))
print(gill_spacing_group_1)
print(gill_spacing_group_2)
gill_spacing_df <- data.frame(x=c("close", "crowded"),
                              n_mushrooms=c(gill_spacing_group_1, gill_spacing_group_2),
                              group=rep(c("1", "2"), each = 2))
ggbarplot(gill_spacing_df, x = "x", y = "n_mushrooms", xlab=c("gill_spacing"), ylab="%",
          fill="group", color="group", position = position_dodge(0.8), lab.col = "group",
          palette = c("#006266", "#C4E538"),
          title = "gill_spacing Proportion", label = TRUE, label.pos = "out", lab.nb.digits = 2)


# gill_size --------------------------
gill_size_group_1 <- prop.table(100*table(data[data$cluster.kmeans ==1,]$gill_size))
gill_size_group_2 <- prop.table(100*table(data[data$cluster.kmeans ==2,]$gill_size))
print(gill_size_group_1)
print(gill_size_group_2)
gill_size_df <- data.frame(x=c("broad", "narrow"),
                           n_mushrooms=c(gill_size_group_1, gill_size_group_2),
                           group=rep(c("1", "2"), each = 2))
ggbarplot(gill_size_df, x = "x", y = "n_mushrooms", xlab=c("gill_size"), ylab="%",
          fill="group", color="group", position = position_dodge(0.8), lab.col = "group",
          palette = c("#006266", "#C4E538"),
          title = "gill_size Proportion", label = TRUE, label.pos = "out", lab.nb.digits = 2)


# gill_color -----------------------
gill_color_group_1 <- prop.table(100*table(data[data$cluster.kmeans ==1,]$gill_color))
gill_color_group_2 <- prop.table(100*table(data[data$cluster.kmeans ==2,]$gill_color))
print(gill_color_group_1)
print(gill_color_group_2)
gill_color_df <- data.frame(x=c("buff", "red", "gray", "chocolate", "black", "brown", "orange", "pink", "green", "purple", "white", "yellow"),
                            n_mushrooms=c(gill_color_group_1, gill_color_group_2),
                            group=rep(c("1", "2"), each = 12))
ggbarplot(gill_color_df, x = "x", y = "n_mushrooms", xlab=c("gill_color"), ylab="%",
          fill="group", color="group", position = position_dodge(0.8), lab.col = "group",
          palette = c("#006266", "#C4E538"),
          title = "gill_color Proportion", label = TRUE, label.pos = "out", lab.nb.digits = 2)


# stalk_shape ---------------------
stalk_shape_group_1 <- prop.table(100*table(data[data$cluster.kmeans ==1,]$stalk_shape))
stalk_shape_group_2 <- prop.table(100*table(data[data$cluster.kmeans ==2,]$stalk_shape))
print(stalk_shape_group_1)
print(stalk_shape_group_2)
stalk_shape_df <- data.frame(x=c("enlarging", "tapering"),
                             n_mushrooms=c(stalk_shape_group_1, stalk_shape_group_2),
                             group=rep(c("1", "2"), each = 2))
ggbarplot(stalk_shape_df, x = "x", y = "n_mushrooms", xlab=c("stalk_shape"), ylab="%",
          fill="group", color="group", position = position_dodge(0.8), lab.col = "group",
          palette = c("#006266", "#C4E538"),
          title = "stalk_shape Proportion", label = TRUE, label.pos = "out", lab.nb.digits = 2)


#stalk_root --------------------------
stalk_root_group_1 <- prop.table(100*table(data[data$cluster.kmeans ==1,]$stalk_root))
stalk_root_group_2 <- prop.table(100*table(data[data$cluster.kmeans ==2,]$stalk_root))
print(stalk_root_group_1)
print(stalk_root_group_2)
stalk_root_df <- data.frame(x=c("missing", "bulbous", "club", "equal", "rooted"),
                            n_mushrooms=c(stalk_root_group_1, stalk_root_group_2),
                            group=rep(c("1", "2"), each = 5))
ggbarplot(stalk_root_df, x = "x", y = "n_mushrooms", xlab=c("stalk_root"), ylab="%",
          fill="group", color="group", position = position_dodge(0.8), lab.col = "group",
          palette = c("#006266", "#C4E538"),
          title = "stalk_root Proportion", label = TRUE, label.pos = "out", lab.nb.digits = 2)


#stalk_surface_above_ring ---------------------
stalk_surface_above_ring_group_1 <- prop.table(100*table(data[data$cluster.kmeans ==1,]$stalk_surface_above_ring))
stalk_surface_above_ring_group_2 <- prop.table(100*table(data[data$cluster.kmeans ==2,]$stalk_surface_above_ring))
print(stalk_surface_above_ring_group_1)
print(stalk_surface_above_ring_group_2)
stalk_surface_above_ring_df <- data.frame(x=c("fibrous", "silky", "smooth", "scaly"),
                                          n_mushrooms=c(stalk_surface_above_ring_group_1, stalk_surface_above_ring_group_2),
                                          group=rep(c("1", "2"), each = 4))
ggbarplot(stalk_surface_above_ring_df, x = "x", y = "n_mushrooms", xlab=c("stalk_surface_above_ring"), ylab="%",
          fill="group", color="group", position = position_dodge(0.8), lab.col = "group",
          palette = c("#006266", "#C4E538"),
          title = "stalk_surface_above_ring Proportion", label = TRUE, label.pos = "out", lab.nb.digits = 2)


#stalk_surface_below_ring ---------------------
stalk_surface_below_ring_group_1 <- prop.table(100*table(data[data$cluster.kmeans ==1,]$stalk_surface_below_ring))
stalk_surface_below_ring_group_2 <- prop.table(100*table(data[data$cluster.kmeans ==2,]$stalk_surface_below_ring))
print(stalk_surface_below_ring_group_1)
print(stalk_surface_below_ring_group_2)
stalk_surface_below_ring_df <- data.frame(x=c("fibrous", "silky", "smooth", "scaly"),
                                          n_mushrooms=c(stalk_surface_below_ring_group_1, stalk_surface_below_ring_group_2),
                                          group=rep(c("1", "2"), each = 4))
ggbarplot(stalk_surface_below_ring_df, x = "x", y = "n_mushrooms", xlab=c("stalk_surface_below_ring"), ylab="%",
          fill="group", color="group", position = position_dodge(0.8), lab.col = "group",
          palette = c("#006266", "#C4E538"),
          title = "stalk_surface_below_ring Proportion", label = TRUE, label.pos = "out", lab.nb.digits = 2)


#stalk_color_above_ring --------------------
stalk_color_above_ring_group_1 <- prop.table(100*table(data[data$cluster.kmeans ==1,]$stalk_color_above_ring))
stalk_color_above_ring_group_2 <- prop.table(100*table(data[data$cluster.kmeans ==2,]$stalk_color_above_ring))
print(stalk_color_above_ring_group_1)
print(stalk_color_above_ring_group_2)
stalk_color_above_ring_df <- data.frame(x = c("buff", "cinnamon", "red", "gray", "brown", "orange", "pink", "white", "yellow"),
                                        n_mushrooms=c(stalk_color_above_ring_group_1, stalk_color_above_ring_group_2),
                                        group=rep(c("1", "2"), each = 9))
ggbarplot(stalk_color_above_ring_df, x = "x", y = "n_mushrooms", xlab=c("stalk_color_above_ring"), ylab="%",
          fill="group", color="group", position = position_dodge(0.8), lab.col = "group",
          palette = c("#006266", "#C4E538"),
          title = "stalk_color_above_ring Proportion", label = TRUE, label.pos = "out", lab.nb.digits = 2)


#stalk_color_below_ring ----------------------------
stalk_color_below_ring_group_1 <- prop.table(100*table(data[data$cluster.kmeans ==1,]$stalk_color_below_ring))
stalk_color_below_ring_group_2 <- prop.table(100*table(data[data$cluster.kmeans ==2,]$stalk_color_below_ring))
print(stalk_color_below_ring_group_1)
print(stalk_color_below_ring_group_2)
stalk_color_below_ring_df <- data.frame(x = c("buff", "cinnamon", "red", "gray", "brown", "orange", "pink", "white", "yellow"),
                                        n_mushrooms=c(stalk_color_below_ring_group_1, stalk_color_below_ring_group_2),
                                        group=rep(c("1", "2"), each = 9))
ggbarplot(stalk_color_below_ring_df, x = "x", y = "n_mushrooms", xlab=c("stalk_color_below_ring"), ylab="%",
          fill="group", color="group", position = position_dodge(0.8), lab.col = "group",
          palette = c("#006266", "#C4E538"),
          title = "stalk_color_below_ring Proportion", label = TRUE, label.pos = "out", lab.nb.digits = 2)


# veil_color ----------------------------
veil_color_group_1 <- prop.table(100*table(data[data$cluster.kmeans ==1,]$veil_color))
veil_color_group_2 <- prop.table(100*table(data[data$cluster.kmeans ==2,]$veil_color))
print(veil_color_group_1)
print(veil_color_group_2)
veil_color_df <- data.frame(x = c("brown", "orange", "white", "yellow"),
                            n_mushrooms=c(veil_color_group_1, veil_color_group_2),
                            group=rep(c("1", "2"), each = 4))
ggbarplot(veil_color_df, x = "x", y = "n_mushrooms", xlab=c("veil_color"), ylab="%",
          fill="group", color="group", position = position_dodge(0.8), lab.col = "group",
          palette = c("#006266", "#C4E538"),
          title = "veil_color Proportion", label = TRUE, label.pos = "out", lab.nb.digits = 2)


#ring_number -----------------------
ring_number_group_1 <- prop.table(100*table(data[data$cluster.kmeans ==1,]$ring_number))
ring_number_group_2 <- prop.table(100*table(data[data$cluster.kmeans ==2,]$ring_number))
print(ring_number_group_1)
print(ring_number_group_2)
ring_number_df <- data.frame(x = c("none", "one", "two"),
                             n_mushrooms=c(ring_number_group_1, ring_number_group_2),
                             group=rep(c("1", "2"), each = 3))
ggbarplot(ring_number_df, x = "x", y = "n_mushrooms", xlab=c("ring_number"), ylab="%",
          fill="group", color="group", position = position_dodge(0.8), lab.col = "group",
          palette = c("#006266", "#C4E538"),
          title = "ring_number Proportion", label = TRUE, label.pos = "out", lab.nb.digits = 2)

#ring_type ---------------------------
ring_type_group_1 <- prop.table(100*table(data[data$cluster.kmeans ==1,]$ring_type))
ring_type_group_2 <- prop.table(100*table(data[data$cluster.kmeans ==2,]$ring_type))
print(ring_type_group_1)
print(ring_type_group_2)
ring_type_df <- data.frame(x = c("evanescent", "flaring", "large", "none", "pendant"),
                           n_mushrooms=c(ring_type_group_1, ring_type_group_2),
                           group=rep(c("1", "2"), each = 5))
ggbarplot(ring_type_df, x = "x", y = "n_mushrooms", xlab=c("ring_type"), ylab="%",
          fill="group", color="group", position = position_dodge(0.8), lab.col = "group",
          palette = c("#006266", "#C4E538"),
          title = "ring_type Proportion", label = TRUE, label.pos = "out", lab.nb.digits = 2)


#spore_print_color -------------------------
spore_print_color_group_1 <- prop.table(100*table(data[data$cluster.kmeans ==1,]$spore_print_color))
spore_print_color_group_2 <- prop.table(100*table(data[data$cluster.kmeans ==2,]$spore_print_color))
print(spore_print_color_group_1)
print(spore_print_color_group_2)
spore_print_color_df <- data.frame(x = c("buff", "chocolate", "black", "brown", "orange", "green", "purple", "white", "yellow"),
                                   n_mushrooms=c(spore_print_color_group_1, spore_print_color_group_2),
                                   group=rep(c("1", "2"), each = 9))
ggbarplot(spore_print_color_df, x = "x", y = "n_mushrooms", xlab=c("spore_print_color"), ylab="%",
          fill="group", color="group", position = position_dodge(0.8), lab.col = "group",
          palette = c("#006266", "#C4E538"),
          title = "spore_print_color Proportion", label = TRUE, label.pos = "out", lab.nb.digits = 2)


#population --------------------------
population_group_1 <- prop.table(100*table(data[data$cluster.kmeans ==1,]$population))
population_group_2 <- prop.table(100*table(data[data$cluster.kmeans ==2,]$population))
print(population_group_1)
print(population_group_2)
population_df <- data.frame(x = c("abundant", "clustered", "numerous", "scattered", "several", "solitary"),
                            n_mushrooms=c(population_group_1, population_group_2),
                            group=rep(c("1", "2"), each = 6))
ggbarplot(population_df, x = "x", y = "n_mushrooms", xlab=c("population"), ylab="%",
          fill="group", color="group", position = position_dodge(0.8), lab.col = "group",
          palette = c("#006266", "#C4E538"),
          title = "population Proportion", label = TRUE, label.pos = "out", lab.nb.digits = 2)


#habitat ----------------------------------
habitat_group_1 <- prop.table(100*table(data[data$cluster.kmeans ==1,]$habitat))
habitat_group_2 <- prop.table(100*table(data[data$cluster.kmeans ==2,]$habitat))
print(habitat_group_1)
print(habitat_group_2)
habitat_df <- data.frame(x = c("woods", "grasses", "leaves", "meadows", "paths", "urban", "waste"),
                         n_mushrooms=c(habitat_group_1, habitat_group_2),
                         group=rep(c("1", "2"), each = 7))
ggbarplot(habitat_df, x = "x", y = "n_mushrooms", xlab=c("habitat"), ylab="%",
          fill="group", color="group", position = position_dodge(0.8), lab.col = "group",
          palette = c("#006266", "#C4E538"),
          title = "habitat Proportion", label = TRUE, label.pos = "out", lab.nb.digits = 2)

