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

# Show dataset dimension
dim(data)

# Rename columns for better comprehension
names(data) <- c("type", "cap_shape", "cap_surface", "cap_color", "has_bruises", "odor", "gill_attachment", "gill_spacing", "gill_size", "gill_color", "stalk_shape", "stalk_root", "stalk_surface_above_ring", "stalk_surface_below_ring", "stalk_color_above_ring", "stalk_color_below_ring", "veil_type", "veil_color", "ring_number", "ring_type", "spore_print_color", "population", "habitat")

# Show list of components of data
names(data)

# Show first 2 rows
head(data, n=2)

# Display internal structure of data
str(data)
sapply(data, typeof)
sapply(data, class)

typeof(data)
class(data)

# N° columns dataset
ncol(data)
# N° rows dataset
nrow(data)

# ---------- Analysis type -------------------------
# type:
#   e -> edible
#   p -> poisonous

# Count observations for each class
type_table <- table(data$type)
print(type_table)

# Get percentage of each class: "e" & "p"
type_percentage <- 100*prop.table(table(data$type))
print(type_percentage)

# Discutir si calcular variancia , media , etc

# dataframe formatted to create barplot
type_df <- data.frame(type=c("edible", "poisonous"), n_mushrooms=c(type_table[1], type_table[2]))

# Barplot of type of 
ggbarplot(type_df, x = "type", y = "n_mushrooms", xlab=c("Type"), ylab="# of Mushrooms", 
          fill="type", color="type", palette = c("#f6f578", "#06623b"), width = c(0.4, 0.4), 
          title = "Consumable Mushrooms", label = TRUE, label.pos = "out")


# ---------- Analysis cap_shape -------------------------
# cap_shape:
#   b -> bell     x -> convex k -> knobbed  
#   c -> conical  f -> flat   s -> sunken

#Count observation per class & type
cap_shape_table <- table(data$type, data$cap_shape)
print(cap_shape_table)

# Get percentage of each class: "b", "c", "f", "k", "s", "x"
cap_shape_percentage <- 100*prop.table(table(data$cap_shape))
print(cap_shape_percentage)

# Dataframe formatted to create barplot of cap_shape
cap_shape_df <- data.frame(cap_shape=rep(c("bell", "conical", "flat", "knobbed", "sunken", "convex"), each = 2), 
                           n_mushrooms=c(cap_shape_table[,1], cap_shape_table[,2], cap_shape_table[,3], 
                                         cap_shape_table[,4], cap_shape_table[,5], cap_shape_table[,6]),
                           type=rep(c("edible", "poisonous"), each = 1))

# Barplot of cap_shape
#ggbarplot(cap_shape_df, x = "cap_shape", y = "n_mushrooms", xlab=c("Cap Shapes"), ylab="# of Mushrooms", 
#          fill="cap_shape", color="cap_shape", palette = c("#ffd31d", "#06623b", "#ffb385", "#ff7272", "#00a1ab", "#00263b"), 
#          title = "Mushroom's Cap Shapes", label = TRUE, label.pos = "out")

# Separated plot by "edible" & "poisonous
ggbarplot(cap_shape_df, x = "cap_shape", y = "n_mushrooms", xlab=c("Cap Shapes"), ylab="# of Mushrooms", 
          fill="type", color="type", position = position_dodge(0.8), lab.col = "type",
          palette = c("#27ae60", "#2c3e50"), 
          title = "Mushroom's Cap Shapes", label = TRUE, label.pos = "out")


# ---------- Analysis cap_surface -------------------------
# cap_surface:
#   f -> fibrous  s -> smooth   
#   g -> grooves  y -> scaly 

#Count observation per class & type
cap_surface_table <- table(data$type, data$cap_surface)
print(cap_surface_table)

#TODO Considerar hacer un gráfico de torta para los % 
# Get percentage of each class: "f", "g, "s", "y"
cap_surface_percentage <- 100*prop.table(table(data$cap_surface))
print(cap_surface_percentage)

# Dataframe formatted to create barplot of cap_surface
cap_surface_df <- data.frame(cap_surface=rep(c("fibrous", "grooves", "smooth", "scaly"), each = 2), 
                           n_mushrooms=c(cap_surface_table[,1], cap_surface_table[,2], cap_surface_table[,3], 
                                         cap_surface_table[,4]),
                           type=rep(c("edible", "poisonous"), each = 1))

# Barplot of cap_surface
# Separated plot by "edible" & "poisonous"
ggbarplot(cap_surface_df, x = "cap_surface", y = "n_mushrooms", xlab=c("Cap Surface"), ylab="# of Mushrooms", 
          fill="type", color="type", position = position_dodge(0.8), lab.col = "type",
          palette = c("#e67e22", "#e74c3c"), 
          title = "Mushroom's Cap Surface", label = TRUE, label.pos = "out")


# ---------- Analysis cap_color -------------------------
# cap_color:
#   b -> buff     e -> red   n -> brown  r -> green  w -> white 
#   c -> cinnamon g -> gray  p -> pink   u -> purple y -> yellow

#Count observation per class & type
cap_color_table <- table(data$type, data$cap_color)
print(cap_color_table)

#TODO Considerar hacer un gráfico de torta para los % 
# Get percentage of each class: "b", "c", "e", "g", "n", "p", "r", "u", "w", "y"
cap_color_percentage <- 100*prop.table(table(data$cap_color))
print(cap_color_percentage)

# Dataframe formatted to create barplot of cap_color
cap_color_df <- data.frame(cap_color=rep(c("buff", "cinnamon", "red", "gray", "brown", "pink", "green", "purple", "white", "yellow"), each = 2), 
                             n_mushrooms=c(cap_color_table[,1], cap_color_table[,2], cap_color_table[,3], cap_color_table[,4], cap_color_table[,5],
                                           cap_color_table[,6], cap_color_table[,7], cap_color_table[,8], cap_color_table[,9], cap_color_table[,10]),
                             type=rep(c("edible", "poisonous"), each = 1))

# Barplot of cap_color
# Separated plot by "edible" & "poisonous"
ggbarplot(cap_color_df, x = "cap_color", y = "n_mushrooms", xlab=c("Cap Color"), ylab="# of Mushrooms", 
          fill="type", color="type", position = position_dodge(0.8), lab.col = "type",
          palette = c("#f39c12", "#34495e"), 
          title = "Mushroom's Cap Color", label = TRUE, label.pos = "out")


# ---------- Analysis has_bruises -------------------------
# has_bruises:
#   f -> false 
#   t -> true

#Count observation per class & type
has_bruises_table <- table(data$type, data$has_bruises)
print(has_bruises_table)

#TODO Considerar hacer un gráfico de torta para los % 
#SACAR % DE LOS COMESTIBLES QUE TIENEN BRUISES ETC... 
# Get percentage of each class: "f", "t"
has_bruises_percentage <- 100*prop.table(table(data$has_bruises))
print(has_bruises_percentage)

# Dataframe formatted to create barplot of has_bruises
has_bruises_df <- data.frame(bruises=rep(c("false", "true"), each = 2), 
                             n_mushrooms=c(has_bruises_table[,1], has_bruises_table[,2]),
                             type=rep(c("edible", "poisonous"), each = 1))

# Barplot of has_bruises
# Separated plot by "edible" & "poisonous"
ggbarplot(has_bruises_df, x = "bruises", y = "n_mushrooms", xlab=c("Has Bruises"), ylab="# of Mushrooms", 
          fill="type", color="type", position = position_dodge(0.8), lab.col = "type",
          palette = c("#f39c12", "#34495e"), 
          title = "Bruises in Mushrooms", label = TRUE, label.pos = "out")


# ---------- Analysis odor -------------------------
# odor:
#   a -> almond   f -> foul   m -> musty  p -> pungent  y -> fishy 
#   c -> creosote l -> anise  n -> none   s -> spicy 

#Count observation per class & type
odor_table <- table(data$type, data$odor)
print(odor_table)

#TODO Considerar hacer un gráfico de torta para los % 
#SACAR % DE LOS COMESTIBLES QUE TIENEN ODOR X ETC... 
# Get percentage of each class: "a", "c", "f", "l", "m", "n", "p", "s", "y"
odor_percentage <- 100*prop.table(table(data$odor))
print(odor_percentage)

# Dataframe formatted to create barplot of odor
odor_df <- data.frame(odor=rep(c("almond", "creosote", "foul", "anise", "musty", "none", "pungent", "spicy", "fishy"), each = 2), 
                      n_mushrooms=c(odor_table[,1], odor_table[,2], odor_table[,3], odor_table[,4], odor_table[,5], odor_table[,6],
                                    odor_table[,7], odor_table[,8], odor_table[,9]),
                      type=rep(c("edible", "poisonous"), each = 1))

# Barplot of odor
# Separated plot by "edible" & "poisonous"
ggbarplot(odor_df, x = "odor", y = "n_mushrooms", xlab=c("Mushroom's odor"), ylab="# of Mushrooms", 
          fill="type", color="type", position = position_dodge(0.8), lab.col = "type",
          palette = c("#fd79a8", "#00cec9"), 
          title = "Mushroom's Odor", label = TRUE, label.pos = "out")


# ---------- Analysis gill_attachment -------------------------
# gill_attachment:
#   a -> attached d -> descending 
#   f -> free     n -> notched

#Notar que el dataset no posee observaciones para las categorias "d", "n" (!)

#Count observation per class & type
gill_attachment_table <- table(data$type, data$gill_attachment)
print(gill_attachment_table)

#TODO Considerar hacer un gráfico de torta para los % 
#SACAR % DE LOS COMESTIBLES QUE TIENEN ODOR X ETC... 
# Get percentage of each class: "a", "f", "d", "n"
gill_attachment_percentage <- 100*prop.table(table(data$gill_attachment))
print(gill_attachment_percentage)

# Dataframe formatted to create barplot of gill_attachment
gill_attachment_df <- data.frame(gill_attachment=rep(c("attached", "free", "descending", "notched"), each = 2), 
                      n_mushrooms=c(gill_attachment_table[,1], gill_attachment_table[,2], 0, 0, 0, 0),
                      type=rep(c("edible", "poisonous"), each = 1))

# Barplot of gill_attachment
# Separated plot by "edible" & "poisonous"
ggbarplot(gill_attachment_df, x = "gill_attachment", y = "n_mushrooms", xlab=c("Mushroom's Gill Attachment"), ylab="# of Mushrooms", 
          fill="type", color="type", position = position_dodge(0.8), lab.col = "type",
          palette = c("#fd79a8", "#00cec9"), 
          title = "Mushroom's Gill Atachment", label = TRUE, label.pos = "out")


# ---------- Analysis gill_spacing -------------------------
# gill_spacing:
#   c -> close   d -> distant 
#   w -> crowded 

#Notar que el dataset no posee observaciones para las categorias "d" (!)

#Count observation per class & type
gill_spacing_table <- table(data$type, data$gill_spacing)
print(gill_spacing_table)

#TODO Considerar hacer un gráfico de torta para los % 
#SACAR % DE LOS COMESTIBLES QUE TIENEN ODOR X ETC... 
# Get percentage of each class: "c", "w", "d"
gill_spacing_percentage <- 100*prop.table(table(data$gill_spacing))
print(gill_spacing_percentage)

# Dataframe formatted to create barplot of gill_attachment
gill_spacing_df <- data.frame(gill_spacing=rep(c("close", "crowded", "distant"), each = 2), 
                              n_mushrooms=c(gill_spacing_table[,1], gill_spacing_table[,2], 0, 0),
                              type=rep(c("edible", "poisonous"), each = 1))

# Barplot of gill_spacing
# Separated plot by "edible" & "poisonous"
ggbarplot(gill_spacing_df, x = "gill_spacing", y = "n_mushrooms", xlab=c("Mushroom's Gill Spacing"), ylab="# of Mushrooms", 
          fill="type", color="type", position = position_dodge(0.8), lab.col = "type",
          palette = c("#00b894", "#0984e3"), 
          title = "Mushroom's Gill Spcing", label = TRUE, label.pos = "out")


# ---------- Analysis gill_size -------------------------
# gill_size:
#   b -> broad    
#   n -> narrow 

#Count observation per class & type
gill_size_table <- table(data$type, data$gill_size)
print(gill_size_table)

#TODO Considerar hacer un gráfico de torta para los % 
#SACAR % DE LOS COMESTIBLES QUE TIENEN ODOR X ETC... 
# Get percentage of each class: "b", "n"
gill_size_percentage <- 100*prop.table(table(data$gill_size))
print(gill_size_percentage)

# Dataframe formatted to create barplot of gill_size
gill_size_df <- data.frame(gill_size=rep(c("broad", "narrow"), each = 2), 
                           n_mushrooms=c(gill_size_table[,1], gill_size_table[,2]),
                           type=rep(c("edible", "poisonous"), each = 1))

# Barplot of gill_size
# Separated plot by "edible" & "poisonous"
ggbarplot(gill_size_df, x = "gill_size", y = "n_mushrooms", xlab=c("Mushroom's Gill Size"), ylab="# of Mushrooms", 
          fill="type", color="type", position = position_dodge(0.8), lab.col = "type",
          palette = c("#a29bfe", "#fdcb6e"), 
          title = "Mushroom's Gill Size", label = TRUE, label.pos = "out")


# ---------- Analysis gill_color -------------------------
# gill_color:
#   b -> buff  g -> gray       k -> black  o -> orange  r -> green   w -> white
#   e -> red   h -> chocolate  n -> brown  p -> pink    u -> purple  y -> yellow

#Count observation per class & type
gill_color_table <- table(data$type, data$gill_color)
print(gill_color_table)

#TODO Considerar hacer un gráfico de torta para los % 
#SACAR % DE LOS COMESTIBLES QUE TIENEN ODOR X ETC... 
# Get percentage of each class: "b", "e", "g", "h", "k", "n", "o", "p", "r", "u", "w", "y"
gill_color_percentage <- 100*prop.table(table(data$gill_color))
print(gill_color_percentage)

# Dataframe formatted to create barplot of gill_color
gill_color_df <- data.frame(gill_color=rep(c("buff", "red", "gray", "chocolate", "black", "brown", "orange", "pink", "green", "purple", "white", "yellow"), each = 2), 
                            n_mushrooms=c(gill_color_table[,1], gill_color_table[,2], gill_color_table[,3], gill_color_table[,4],
                                          gill_color_table[,5], gill_color_table[,6], gill_color_table[,7], gill_color_table[,8],
                                          gill_color_table[,9], gill_color_table[,10], gill_color_table[,11], gill_color_table[,12]),
                            type=rep(c("edible", "poisonous"), each = 1))

# Barplot of odor
# Separated plot by "edible" & "poisonous"
# Barplot divided in two parts for better visualization
ggbarplot(gill_color_df %>% slice(1:12), x = "gill_color", y = "n_mushrooms", xlab=c("Color"), ylab="# of Mushrooms", 
          fill="type", color="type", position = position_dodge(0.8), lab.col = "type",
          palette = c("#B53471", "#F79F1F"), 
          title = "Mushroom's Gill Color", label = TRUE, label.pos = "out")

ggbarplot(gill_color_df %>% slice(12:24), x = "gill_color", y = "n_mushrooms", xlab=c("Color"), ylab="# of Mushrooms", 
          fill="type", color="type", position = position_dodge(0.8), lab.col = "type",
          palette = c("#B53471", "#F79F1F"), 
          title = "Mushroom's Gill Color", label = TRUE, label.pos = "out")


# ---------- Analysis stalk_shape -------------------------
# stalk_shape:
#   e -> enlarging    
#   t -> tapering 

#Count observation per class & type
stalk_shape_table <- table(data$type, data$stalk_shape)
print(stalk_shape_table)

#TODO Considerar hacer un gráfico de torta para los % 
#SACAR % DE LOS COMESTIBLES QUE TIENEN ODOR X ETC... 
# Get percentage of each class: "e", "t"
stalk_shape_percentage <- 100*prop.table(table(data$stalk_shape))
print(stalk_shape_percentage)

# Dataframe formatted to create barplot of gill_size
stalk_shape_df <- data.frame(stalk_shape=rep(c("enlarging", "tapering"), each = 2), 
                             n_mushrooms=c(stalk_shape_table[,1], stalk_shape_table[,2]),
                             type=rep(c("edible", "poisonous"), each = 1))

# Barplot of gill_size
# Separated plot by "edible" & "poisonous"
ggbarplot(stalk_shape_df, x = "stalk_shape", y = "n_mushrooms", xlab=c("Stalk Shape"), ylab="# of Mushrooms", 
          fill="type", color="type", position = position_dodge(0.8), lab.col = "type",
          palette = c("#006266", "#C4E538"), 
          title = "Mushroom's Stalk Shape", label = TRUE, label.pos = "out")


# ---------- Analysis stalk_root -------------------------
# stalk_root:
#   ? -> missing   c -> club   r -> rooted        u-> cup
#   b -> bulbous   e -> equal  z -> rhizomorphs

#Notar que el dataset no posee observaciones para las categorias "z", "u" (!)

#Count observation per class & type
stalk_root_table <- table(data$type, data$stalk_root)
print(stalk_root_table)

#TODO Considerar hacer un gráfico de torta para los % 
#SACAR % DE LOS COMESTIBLES QUE TIENEN ODOR X ETC... 
# Get percentage of each class: "?", "b", "c", "e", "r", "z", "u"
stalk_root_percentage <- 100*prop.table(table(data$stalk_root))
print(stalk_root_percentage)

print(stalk_root_table[,2])
# Dataframe formatted to create barplot of stalk_root
stalk_root_df <- data.frame(stalk_root=rep(c("missing", "bulbous", "club", "equal", "rooted", "rhizomorphs", "cup"), each = 2), 
                            n_mushrooms=c(stalk_root_table[,1], stalk_root_table[,2], stalk_root_table[,3], stalk_root_table[,4], stalk_root_table[,5], 
                                          0, 0, 0, 0),
                            type=rep(c("edible", "poisonous"), each = 1))

# Barplot of stalk_root
# Separated plot by "edible" & "poisonous"
ggbarplot(stalk_root_df, x = "stalk_root", y = "n_mushrooms", xlab=c("Stalk Root"), ylab="# of Mushrooms", 
          fill="type", color="type", position = position_dodge(0.8), lab.col = "type",
          palette = c("#D980FA", "#9980FA"), 
          title = "Mushroom's Stalk Root", label = TRUE, label.pos = "out")


# ---------- Analysis stalk_surface_above_ring -------------------------
