# 1. Extract species data for first quartile (ie most sensitive species) 

# Create a list of carbamates (there should be 5)
all.chem.carbamate <- unique(data$Chemical)

# Create an empty list for the most sensitive species
most.sensitive.species <- list()

# Create a loop to iterate through all the chemicals in the set
for(i in all.chem.carbamate) {

  #Calculating the average toxicity value (LC50) for each species 
  IndividualSSDs <- data %>% 
    filter(Chemical == i) %>%
    group_by(Species) %>%
    summarise(
      average = geoMean(Conc), # here I am using "average" instead of "geomean" as earlier
      minimum = min(Conc),
      sd = sd(Conc),
      n = n())
  
  IndividualSSDs <- unique(IndividualSSDs) #Adding toxicity data for each chemical to a list and making sure there are no duplicates
  
  # Create intermediate data frame
  Newdf <- IndividualSSDs
  
  #Calculating the probability points for each species and order them
  df <- Newdf[order(Newdf$average),]
  df$frac <- ppoints(df$average, 0.5)
  
  # Create intermediate data frame and filter by the lowest quartile
  most.sensitive.sp <- df %>%
  filter(
    average <= quantile(df$average, probs = 0.25))
  
  # Combine the data for this chemical into a data frame
  most.sensitive.sp.table <- data.frame(i, most.sensitive.sp)
  
  # Save this run of the loop as a place in the list
  most.sensitive.species[[i]] <- most.sensitive.sp.table
}

# Bind all of the elements of the list together into one data frame
most.sensitive.species <- rbindlist(most.sensitive.species)

# Rename the column names
colnames(most.sensitive.species) <- c("Chemical", "Species", "average", "minimum", "sd", "n", "frac")
