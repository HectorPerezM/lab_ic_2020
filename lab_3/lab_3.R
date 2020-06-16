#Laboratorio N°3

#Autores:
#   - Nicolás Alarcón L.
#   - Pedro Silva A.
#   - Héctor Pérez M.

#Se cargan packages
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

#Create "One-hot-encoding" data.frame
#dummy <- dummyVars("~ .", data = data)
#data_ohe <- data.frame(predict(dummy, newdata = data))
#https://blog.aptitive.com/building-the-transactions-class-for-association-rule-mining-in-r-using-arules-and-apriori-c6be64268bc4
#data_ohe[data_ohe=="1"]<-"TRUE"
#data_ohe[data_ohe=="0"]<-"FALSE"

data_trans <- as(data, "transactions")

#Create rules
rules <- apriori(data = data_trans, appearance = list(rhs=c("type=edible"), default='lhs'), parameter = list(supp = 0.1, conf = .1))

#Sortings
rules <- sort(rules, by = "lift", decreasing = T)
inspect(rules[1:20])

#Graphs
#http://finzi.psych.upenn.edu/library/arulesViz/doc/arulesViz.pdf
#Cuidado al correr, es muy pesado
#plot(rules)
plot(rules[1:20], method="graph", control=list(type="items"))
plot(rules, method = "grouped")
plot(rules, method = "grouped", control = list(k = 50))

sel <- plot(rules,
            measure=c("support", "lift"),
            shading = "confidence",
            interactive = TRUE)

#plot(rules.all)
#plot(rules.all, method = "grouped")
#plot(rules.all, method = "graph")
#plot(rules.all, method = "graph", control = list(type="items"))
#plot(rules.all, method = "parcoord", control = list(reorder=TRUE))
