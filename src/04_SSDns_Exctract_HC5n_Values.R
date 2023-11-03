We have calculated the single-chem HC5s, the single-chem LOO HC5s, and now we can move on to the SSDn HC5s.

# 4. Build normalized SSDs and extract the HC5n/HC5 values
#Using this method, we will create the maximum number of SSDns possible, by first creating the maximum number of combinations of chemicals and by using all possible nSpecies within each combination.

### Initialize relevant variables before loop

# Create a list of the carbamate chemicals we are using
list.carbamates <- c("Carbofuran", "Methomyl", "Aminocarb", "Carbaryl", "Mexacarbate")

#Creates a list of different random draws from the 5 chemicals; each is for a different number of chemicals being drawn (m), and each draws the number of times (n) without repetition. Assigns the data.tables of these different chemical combinations to a variable called "chem.list.carbamate".
chem.list.carbamate <- list(
  data.table(comboSample(list.carbamates, m = 2, n = 10, repetition = FALSE)),
  data.table(comboSample(list.carbamates, m = 3, n = 10, repetition = FALSE)), 
  data.table(comboSample(list.carbamates, m = 4, n = 5, repetition = FALSE)),
  data.table(comboSample(list.carbamates, m = 5, n = 1, repetition = FALSE)))

#check
chem.list.carbamate[[1]]


### Initialize the loop 

# Initialize the data vectors - these are empty lists that will store the outputs of the loop so we can combine them later
statvec <- c()
numbers <- c()
Compounds <- list()
List.Statsdf <- list()
List.Compounds <- list()

# This loop iterates through the groupings of chemicals: in this case, we have our list of carbamates with 4 different groupings (2 chems, 3 chems, 4 chems, 5 chems). 
for (y in 1:4) {
  
  current.chem.list <- chem.list.carbamate[[y]] # index the current chemicals
  Statsdf <- list() # make a list to hold the outputs
  xlength <- length(current.chem.list$V1) # finds how many different groups there are within that group (ie there are 10 pairs of 2 chems)

# This next loop is nested within the first loop. It takes the chemicals within the group we are looking at, makes a data frame matching those chemicals
for (x in 1:xlength) {
  
  carbamate.chemicals <- current.chem.list[x]
  
#Making a dataframe using only chemicals with the same MOA, for a single water type
carbamate.data <- data %>%
  filter(Chemical %in% carbamate.chemicals)

#Select species from each compound of interest
ListByChemical <- list()

# This loop iterates through each chemical in our list.
for (Chemical_ in carbamate.chemicals){
  
  # Set the current chemical to the chemical of interest
  CurrentCompound <- Chemical_
  
  # Filter the species to include only species from the current compound of all the species in the data set
  CurrentCompoundSpecies <- carbamate.data %>%
    filter(Chemical == CurrentCompound & Species %in% data$Species)
  
  #Makes a list of species but each species is only represented one time
  SingleList<- unique(CurrentCompoundSpecies$Species)
  
  #This assigns the newly made vector of species for a chemical to the next item of the list. The double bracket [[i]] is the index number of where it is located in that list.
  ListByChemical[[Chemical_]] <- SingleList
  
}

# Create a vector of species that are shared across all chemicals (ie our nSpecies pool)
xspecies <- Reduce(intersect, ListByChemical)

# The next loop iterates through this nSpecies pool and sets each as the normalization species, so it can be used to calculate the LC50n values

# initialize list for holding data
byspecies.df <- list()

for(xspecies_ in xspecies){

  # Set the normalization species
  commonspecies <- xspecies_ 
  
  # Create a data frame of toxicity data for just the normalization species (eg only daphnia magna, for example)
  ToxSensitive <- carbamate.data %>%
    filter(Species == commonspecies)

  # Calculate the geometric mean of the nSpecies toxicity (also called the SMAV of the nSpecies)
  ToxSensitive <- ToxSensitive %>%
    group_by(Chemical) %>% # group by each chemical
    summarise(
    average = geoMean(Conc)) #SMAV of the nSpecies

# We now have the nSpecies SMAVs calculated for each chemical. The next loop will calculate LC50n values for one chemical group at a time. It will assign them to a list of data frames. To calculate LC50n values you divide the LC50 by the nSpecies SMAV
# LC50n values are calculated by dividing the LC50 by the normalizing species 

# Set up empty lists for holding data
df.bf <- list()
chem.no.spec <- list()

# This loop iterates through each chemical - for each chemical in the list of carbamates:
for(Chemical_ in carbamate.chemicals){
  
  # Filter the data for only that chemical
  ICE1 <- carbamate.data %>%
    filter(Chemical == Chemical_)
  
  # Save the nSpecies SMAV for that chemical as the average value
  avgval <- ToxSensitive$average[ToxSensitive$Chemical == Chemical_]
  
  # Calculate the SMAV for each species for a SINGLE chemical
  ICE2 <- ICE1 %>%
    group_by(Species) %>% # group by species to account for multiple observations
    summarise(
      SMAV = geoMean(Conc), # Calculate the SMAV for each species for a single chemical
      Genus = Genus
    ) %>%
    mutate(SMAV.norm = SMAV/avgval) # Normalize SMAVs for each species for a single chemical using the average value of the nSpecies
  
  # Remove duplicate values
  ICE2 <- unique(ICE2)
  
  # Save the results into the individual chemical's list
  df.bf[[Chemical_]] <- ICE2
  
  # Order the species for this chemical by the SMAV
  df.nSpecies <- ICE2[order(ICE2$SMAV.norm),]
  
  # Find the cumulative probability of each species SMAV (as you would in a regular SSD)
  df.nSpecies$P <- ppoints(df.nSpecies$SMAV.norm, 0.5)
  
  # Find the cumulative probability of the nSpecies for that chemical
  nSpecies.P.chem <- df.nSpecies$P[df.nSpecies$Species == commonspecies]
  
  # Save that into a data frame
  data.frame(nSpecies.P.chem)
  
  # Save all of this data into the list
  chem.no.spec[[Chemical_]] <- data.table(Chemical_, length(ICE2$SMAV.norm), length(unique(ICE2$Genus)), nSpecies.P.chem)
  
}

# These tables combine all the separate tables with LC50n values
Newdf.bf <- rbindlist(df.bf)
chem.no.spec <- rbindlist(chem.no.spec)
colnames(chem.no.spec) <- c("Chemical", "no.species", "no.genera", "nSpecies.P.chem")
chem.no.spec <- chem.no.spec %>%
  mutate(nSpecies.P.chem.SEM = std.error(nSpecies.P.chem))
tot.data <- length(Newdf.bf$Species)


# We now have all the LC50n values for all fish species and all chemicals


# This next section of the loop creates the SSDn using the LC50n values for all of the compounds

# Derive the geometric mean of normalized SMAVs across all chemicals
Newdf.bf <- Newdf.bf %>%
  group_by(Species) %>%
  summarise(
    SMAV.norm = geoMean(SMAV.norm), # geometric mean of normalized SMAV
    Genus = Genus,
    n = n())

# Remove any duplicates
Newdf.bf <- unique(Newdf.bf)

# Order by normalized SMAV and find cumulative probability
df <- Newdf.bf[order(Newdf.bf$SMAV.norm),]
df$frac <- ppoints(Newdf.bf$SMAV.norm, 0.5)

# Make it a data table
df <- data.table(df)

# Fit 4 different distributions to the data
fit_llogis <- fitdist(df$SMAV.norm, "llogis") # log logistic distribution
fit_lnorm <- fitdist(df$SMAV.norm, "lnorm") # log normal distribution
fit_gamma <- try(fitdist(df$SMAV.norm, "gamma", silent = TRUE)) # gamma distribution
fit_weibull <- fitdist(df$SMAV.norm, "weibull") # weibull distribution

# Ensure that there are no errors with the gamma distribution; if there is an error, try the other three
if("try-error" %in% class(fit_gamma)){
  ll <- gofstat(fit_llogis)
  ln <- gofstat(fit_lnorm)
  w <- gofstat(fit_weibull)
  
  # Fit the best distribution to the data; identify it by the lowest AIC (this is without gamma)
  a <- unlist(c(ll[8], ln[8], w[8]))
  best.fit.table <- data.table(c("llogis", "lnorm", "weibull"), a)
  m <- min(a)
  b <- best.fit.table$V1[best.fit.table$a == m]
} else {
  ll <- gofstat(fit_llogis)
  ln <- gofstat(fit_lnorm)
  g <- gofstat(fit_gamma)
  w <- gofstat(fit_weibull)
  
  #Fit the best distribution to the data; identify it by the lowest AIC (with gamma included)
  a <- unlist(c(ll[8], ln[8], g[8], w[8]))
  best.fit.table <- data.table(c("llogis", "lnorm", "gamma", "weibull"), a)
  m <- min(a)
  b <- best.fit.table$V1[best.fit.table$a == m]}

# Calculate the best fit distribution
best.fit <- fitdist(df$SMAV.norm, b)

# Save the distribution parameters
params <- names(best.fit[[1]])
params.no <- best.fit[[1]]
param1 <- params[1]
param2 <- params[2]
param1.no <- params.no[1]
param2.no <- params.no[2]

# List of the distribution parameters SDs
params.sd <- best.fit[[3]]
param1.sd <- params.sd[1]
param2.sd <- params.sd[2]

# Calculate a normalized hc5 value with a bootstrap
hc5n <- quantile(best.fit, probs = 0.05)
hc5n <- hc5n$quantiles$`p=0.05`

# Combine all the data from a single run together
data.table(ToxSensitive)
data.table(chem.no.spec)

# Create a joint data frame with the following variables
jointdf <- full_join(ToxSensitive, chem.no.spec, by = "Chemical")
jointdf$iteration <- x # save which iteration it was calculated on
jointdf$bf.HC5n <- hc5n # the hc5n of the SSDn
jointdf$bf.HC5 <- jointdf$average * hc5n #the chemical specific HC5 using the best fit distribution 
jointdf$no.chems <- length(ToxSensitive$Chemical) # number of chemicals used to make the SSDn
jointdf$dist <- b #type of distribution
jointdf$param1 <- param1 # model parameters
jointdf$param1.no <- param1.no # model parameters
jointdf$param1.sd <- param1.sd # model parameters
jointdf$param2 <- param2 # model parameters
jointdf$param2.no <- param2.no # model parameters
jointdf$param2.sd <- param2.sd # model parameters
jointdf$tot.data <- tot.data # total number of SMAVs contributed to the SSDn
jointdf$AIC <- m # Anderson darling stat for the SSDn best fit distribution
jointdf$nSpecies.P.SSDn <- df$frac[df$Species == commonspecies] # cumulative probability of the nSpecies in the SSDn
jointdf$nSpecies <- commonspecies # which nSpecies used in this iteration
jointdf$tot.species <- length(df$Species)# How many species in the SSDn
jointdf$tot.genera <- length(unique(df$Genus)) #How many genera in the SSDn

# index this data to which species made the SSDn
byspecies.df[[xspecies_]] <- jointdf

}

# Bind the results for that run to a spot in the list
Statsdf[[x]] <- rbindlist(byspecies.df)


}
  List.Statsdf[[y]] <- rbindlist(Statsdf) #Add to the overall output dataframe list for each run with diff. no. of chems
}

# Save it all into one dataframe
List.Statsdf.combo <- rbindlist(List.Statsdf)

#The "List.Statsdf.combo" is the raw output of all the SSDns we created using the loop. Before moving on to further analyses, it will be helpful to calculate relevant statistics and variables.

### Calculate means, SDs, SEs, and 95% CIs for the nSpecies and overall

#We are interested in the overall chemical mean rather than the nSpecies means as some species have a lot more data than others; if we only used each value to calculate the overall mean, we would weight the results to the species with more data - which we do not want to do.
  
# First, calculate the means, standard deviations, standard errors, and 95% confidence intervals around the nSpecies mean and overall chemical mean
Final.Data <- List.Statsdf.combo %>%
  dplyr::group_by(Chemical, nSpecies) %>% # first group by chemical then group by the nSpecies
  mutate(
    nspecies.mean.HC5 = mean(bf.HC5)) %>% # Calculate the mean HC5 for that nSpecies
  dplyr::ungroup() %>% # ungroup
  dplyr::group_by(Chemical) %>% # group by chemical
  mutate(
    chemical.mean.HC5 = mean(nspecies.mean.HC5), # calculate the overall average HC5 for that chemical (take the mean of a mean)
    chemical.sd.HC5 = sd(nspecies.mean.HC5), # calculate overall SD for each chemical
    no.obs.length = length(unique(nSpecies)), # calculate how many nSpecies were used for each chemical
    chemical.std.err.HC5 = (chemical.sd.HC5 / sqrt(no.obs.length)), # calculate standard error by dividing the SD by the square root of the sample size - in this case, how many nspecies there are 
    upper.CI = (chemical.mean.HC5 + 1.96*chemical.std.err.HC5), # calculate upper 95% CI
    lower.CI = (chemical.mean.HC5 - 1.96*chemical.std.err.HC5), # calculate lower 95% CI
    upper.SD.limit = (chemical.mean.HC5 + chemical.sd.HC5), # calculate mean + 1 SD
    lower.SD.limit = (chemical.mean.HC5 - chemical.sd.HC5)) %>% # mean - 1 SD
  dplyr::ungroup() # ungroup chemical so data is back how it was

### Add the single chemical HC5s to the final dataset
#Add the single chemical HC5s (true HC5s) to the data - this is helpful for a few of the plots

# Extract the chemical specific HC5s
single.hc5.table <- single.individual.HC5s %>%
  dplyr::select(Chemical, `SSD Estimated HC5`)
  
# Rename the columns so I can join to the full dataset
colnames(single.hc5.table) <- c("Chemical", "Single.C.HC5.bf")

# Join to full data set
Final.Data <- left_join(Final.Data, single.hc5.table, by = "Chemical")
