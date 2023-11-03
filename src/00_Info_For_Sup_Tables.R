### Extract relevant information for supplementary tables
#How many species are there per chemical, what are range of toxicity values, etc.

# Minimum tox value for overall dataset
min(data$Conc) # 1.7

# Maximum tox value for overall dataset
max(data$Conc)

### Aminocarb

# How many species are there per chemical?
print(paste("There are", length(unique(data$Species[data$Chemical == "Aminocarb"])), "species in Aminocarb"))

### Carbaryl

# How many species are there per chemical?
print(paste("There are", length(unique(data$Species[data$Chemical == "Carbaryl"])), "species in Carbaryl"))

### Carbofuran

# How many species are there per chemical?
print(paste("There are", length(unique(data$Species[data$Chemical == "Carbofuran"])), "species in Carbofuran"))


### Methomyl
# How many species are there per chemical?
print(paste("There are", length(unique(data$Species[data$Chemical == "Methomyl"])), "species in Methomyl"))


### Mexacarbate
# How many species are there per chemical?
print(paste("There are", length(unique(data$Species[data$Chemical == "Mexacarbate"])), "species in Mexacarbate"))

