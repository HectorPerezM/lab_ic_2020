#Laboratorio N°3

#Autores:
#   - Nicolás Alarcón L.
#   - Pedro Silva A.
#   - Héctor Pérez M.

#libraries
library(arulesViz)
library(caret)

workdir_path <- "~/code/github.com/HectorPerezM/lab_ic_2020/lab_3"
setwd(workdir_path)

# Dataset path
dataset_path <- "../dataset/mushroom/agaricus-lepiota.data"

# Load dataset
data <- read.csv(dataset_path, header = FALSE)

names(data) <- c("type", "cap_shape", "cap_surface", "cap_color", "has_bruises", "odor", "gill_attachment", "gill_spacing", "gill_size", "gill_color", "stalk_shape", "stalk_root", "stalk_surface_above_ring", "stalk_surface_below_ring", "stalk_color_above_ring", "stalk_color_below_ring", "veil_type", "veil_color", "ring_number", "ring_type", "spore_print_color", "population", "habitat")


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


#Pre-processing

#Remove stalk_root = "missing"
data<-data[!(data$stalk_root == "missing"),]

#Eliminate veil_type
data <- data[,-17]

#Convert data frame to transactions
data_trans <- as(data, "transactions")

#Reglas ---------------------------------------

#Caso 1
#supp=0.1, conf=0.1, ordered by confidence, support

#Caso de todas las reglas
rules <- apriori(data = data_trans, 
                 appearance = list(rhs=c("type=poisonous"), default='lhs'), 
                 parameter = list(supp = 0.1, conf = .1))

#Cantidad de reglas sin filtrar
length(rules)

#Ordenar
ordered_rules <- sort(rules, by = c("confidence","support"), decreasing = T)

#Eliminar reglas redundantes
final_rules <- ordered_rules[!is.redundant(ordered_rules)]

#Cantidad de reglas filtradas
length(final_rules)

#Inspeccionar primeras 10 reglas
inspect(final_rules[1:10])

#Graphs
plot(final_rules, measure = c("support", "confidence"), shading = "lift")
plot(final_rules[1:10], method="graph")
plot(final_rules, method = "grouped")

#Caso 2
#supp=0.1, conf=0.8, ordered by support
#maxlen=13
rules_case2 <- apriori(data = data_trans, 
                       appearance = list(rhs=c("type=poisonous"), default='lhs'), 
                       parameter = list(supp = 0.1, conf = 1))

#Cantidad de reglas sin filtrar
length(rules_case2)

#Ordenar
ordered_rules_case2 <- sort(rules_case2, by = c("support"), decreasing = T)

#Eliminar reglas redundantes
final_rules_case2 <- ordered_rules_case2[!is.redundant(ordered_rules_case2)]

#Cantidad de reglas filtradas
length(final_rules_case2)

#Inspeccionar primeras 10 reglas
inspect(final_rules_case2[1:10])

#Graphs
plot(final_rules_case2, measure = c("support", "confidence"), shading = "lift")
plot(final_rules_case2[1:10], method="graph")
plot(final_rules_case2, method = "grouped")


#Caso 3
#supp=0.20, conf=1, ordered by support
#maxlen=13
rules_case3 <- apriori(data = data_trans, 
                 appearance = list(rhs=c("type=poisonous"), default='lhs'), 
                 parameter = list(supp = 0.20, conf = 1))

#Cantidad de reglas sin filtrar
length(rules_case3)

#Ordenar
ordered_rules_case3 <- sort(rules_case3, by = c("support"), decreasing = T)

#Eliminar reglas redundantes
final_rules_case3 <- ordered_rules_case3[!is.redundant(ordered_rules_case3)]

#Cantidad de reglas filtradas
length(final_rules_case3)

#Inspeccionar primeras 10 reglas
inspect(final_rules_case3[1:10])

#Graphs
plot(final_rules_case3, measure = c("support", "confidence"), shading = "lift")
plot(final_rules_case3[1:10], method="graph")
plot(final_rules_case3, method = "grouped")


#Caso 4
#supp=0.25, conf=1, ordered by support
#maxlen=13
rules_case4 <- apriori(data = data_trans, 
                       appearance = list(rhs=c("type=poisonous"), default='lhs'), 
                       parameter = list(supp = 0.25, conf = 1, minlen=3, maxlen=13))

#Cantidad de reglas sin filtrar
length(rules_case4)

#Ordenar
ordered_rules_case4 <- sort(rules_case4, by = c("support"), decreasing = T)

#Eliminar reglas redundantes
final_rules_case4 <- ordered_rules_case4[!is.redundant(ordered_rules_case4)]

#Cantidad de reglas filtrado
length(final_rules_case4)

#Inspeccionar primeras 10 reglas
inspect(final_rules_case4[1:10])

#Graphs
plot(final_rules_case4, measure = c("support", "confidence"), shading = "lift")
plot(final_rules_case4[1:10], method="graph")
plot(final_rules_case4, method = "grouped")


#Caso 5
#supp=0.25, conf=1, ordered by support
#maxlen=13
rules_case5 <- apriori(data = data_trans, 
                         appearance = list(rhs=c("type=poisonous"), default='lhs'), 
                         parameter = list(supp = 0.25, conf = 1, minlen=6, maxlen=13))

#Cantidad de reglas sin filtrar
length(rules_case5)

#Ordenar
ordered_rules_case5 <- sort(rules_case5, by = c("support"), decreasing = T)

#Eliminar reglas redundantes
final_rules_case5 <- ordered_rules_case5[!is.redundant(ordered_rules_case5)]

#Cantidad de reglas filtradas
length(final_rules_case5)


#Inspeccionar primeras 10 reglas
inspect(final_rules_case5[1:10])

#Graphs
plot(final_rules_case5, measure = c("support", "confidence"), shading = "lift")
plot(final_rules_case5[1:10], method="graph")
plot(final_rules_case5, method = "grouped")
