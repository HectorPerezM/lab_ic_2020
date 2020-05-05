# Laboratorio N°1

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

# Show list of components of data
names(data)

# Show first few rows
head(data)

# Display internal structure of data
str(data)
sapply(data, typeof)
sapply(data, class)

# Histogram of "e: edible" & "p: poisonous"
barplot(width=c(5, 10), space=0, names.arg=c("Comestible", "Venenoso"), table(data["V1"]), legend="Histograma de Hongos 'Comestibles' v/s 'Venenosos'", col = "BLUE")

# Calculate Var
#numeric_v1 <- as.numeric(as.character(data["V1"])) 
s <- as.numeric(unlist(data["V1"]))
# Get Var & SD of V1
var(s)
sd(s)

hist(s, freq=TRUE, col="RED")

# Shows variable info of column "V1"
round(addmargins(a))

# Shows % of "edible" & "poisonous"
# Multiply by 100 to adjust
v1_percentage <- 100*prop.table(table(data["V1"]))
cat(v1_percentage)
