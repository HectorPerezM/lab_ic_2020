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

# ---------- Analysis V1 -> type -------------------------
# type:
#   e -> edible
#   p -> poisonous

# Count observations for each class
type_table <- table(data$type)
print(type_table[1])

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
