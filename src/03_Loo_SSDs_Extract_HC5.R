#We have calculated the single-chemical HC5s, which are a good baseline and represent the "classic" SSD approach. Now, I will use the leave-one-out (LOO) approach to calculate the HC5s along with variance estimates.
# 3. Build and calculate LOO single-chemical SSDs and extract HC5 values.
#The LOO approach we decided to use was to first make an SSD with the maximum possible number of species (N). Then, remove one species from the dataset so that the total number of species is (N-1). Using the dataset of (N-1) species, we make another SSD. We would continue to do that until all possible combinations of (N-1) species have been used to make an SSD.

#In short, if there were N total species in the original chemical data, then the total number of single-chemical SSDs we could make using this method are N (N possible combinations of N-1 species)

### LOO Loop Generation
#NOTE: The warning/error messages it may give are only because the code fits all four distributions (and most of the time, the gamma distribution gives an error). The code fits the best-fit distribution (which does not show an error code)

# Create a list of chemicals from my data set
single.ssd.chems <- c("Aminocarb", "Mexacarbate", "Methomyl", "Carbofuran", "Carbaryl")

# Initialize the lists that will hold the outputs for the chemicals
individual.hc5s.for.chem <- list()
individual.hc5s <- list()

# Create the loop (it is very similar to creating a normal SSD)
for(i in single.ssd.chems) {
  
  #filter by the chemical
  IndividualSSDs <- data %>% 
    filter(Chemical == i)
  
  # get a list of species and store the length as a variable
  species.list <- unique(IndividualSSDs$Species)
  species.list.length <- length(species.list)

  # create all possible species combinations (such that there are N combinations of N-1 species)
  species.combinations <- list(
  data.table(comboSample(species.list, m = (species.list.length-1), n = species.list.length, repetition = FALSE)))

  # bind all the species combos together
  species.combos.data <- rbindlist(species.combinations)
  
  # initialize a list to hold the species 
  overall.sp.list <- list()
  
  # iterate through and create a species list
  for (j in 1:(length(species.combos.data)+1)){
  # Take the current row (j) and save it as a character vector into a list  
    current.sp.list <- as.character(species.combos.data[j,])
    overall.sp.list[[j]] <- current.sp.list
  }
  
  # Initialize the iterating through sp combinations list
  for (k in 1:length(overall.sp.list)){
  
  # Empty variables for storing output
  fit_gamma <- NULL
  best.fit <- NULL
  
  # Index the current list of species
  current.sp.list <- overall.sp.list[[k]]
  
  # Filter the chemical SSD data by the species that match
  chemical.sp.data <- IndividualSSDs %>%
    filter(Species %in% current.sp.list)
  
  # Filter the species that are left out and save into the data
  sp.left.out <- IndividualSSDs %>%
    dplyr::select(Species) %>%
    filter(!Species %in% chemical.sp.data$Species)
  sp.left.out <- sp.left.out[1,1]
  output.list <- chemical.sp.data
  output.list$Sp.left.out <- sp.left.out

  # Calculate the geomeans and metrics needed to create the SSD (same as single-chemical SSD)
  IndividualSSDs.2 <- output.list %>%
    group_by(Species) %>%
    summarise(
      average = geoMean(Conc), 
      minimum = min(Conc),
      sd = sd(Conc),
      n = n(),
      Chemical = Chemical,
      Sp.left.out = Sp.left.out)

  
  IndividualSSDs.2 <- unique(IndividualSSDs.2) #Adding toxicity data for each chemical to a list and making sure there are no duplicates
  
  # Create intermediate data frame
  Newdf <- IndividualSSDs.2
  
  #Calculating the probability points for each species 
  df <- Newdf[order(Newdf$average),]
  df$frac <- ppoints(df$average, 0.5)
  df
  
  #Fit a distribution to the plotted points
  fit_llogis <- fitdist(df$average, "llogis")
  fit_lnorm <- fitdist(df$average, "lnorm")
  fit_gamma <- try(fitdist(df$average, "gamma"), silent = TRUE)
  fit_weibull <- fitdist(df$average, "weibull")
  
  if("try-error" %in% class(fit_gamma)){
    ll <- gofstat(fit_llogis)
    ln <- gofstat(fit_lnorm)
    w <- gofstat(fit_weibull)
    
    #Fit the best distribution to the data; identify it by the lowest anderson darling statistic
    a <- unlist(c(ll[8], ln[8], w[8]))
    best.fit.table <- data.table(c("llogis", "lnorm", "weibull"), a)
    m <- min(a)
    b <- best.fit.table$V1[best.fit.table$a == m]
  }  else {
    ll <- gofstat(fit_llogis)
    ln <- gofstat(fit_lnorm)
    g <- gofstat(fit_gamma)
    w <- gofstat(fit_weibull)
    
    #Fit the best distribution to the data; identify it by the lowest anderson darling statistic
    a <- unlist(c(ll[8], ln[8], g[8], w[8]))
    best.fit.table <- data.table(c("llogis", "lnorm", "gamma", "weibull"), a)
    m <- min(a)
    b <- best.fit.table$V1[best.fit.table$a == m]}
  
  #Calculate the best fit distribution
  best.fit <- fitdist(df$average, b)
  
  # Extract the estimated HC5 value
  hc5 <- quantile(best.fit, probs = 0.05)
  Estimated_HC5 <- hc5$quantiles$`p=0.05`
  
  # Bootstrap the HC5 value with upper and lower CIs
  fit_boot <- bootdist(best.fit, bootmethod = 'param', niter = 1000)
  
  # This step is not necessary I am just using it to check the bootstrap is working
  bootstrap_hc5 <- quantile(fit_boot, probs = 0.05)
  
  # Save boostrapped HC5 and CIs
  Bootstrapped_HC5 <- bootstrap_hc5$quantiles$`p=0.05`
  HC5_lower_ci <- bootstrap_hc5$quantCI[1,1]
  HC5_upper_ci <- bootstrap_hc5$quantCI[2,1]
  
  # Save the identifying variables
  Chem.ID <- unique(df$Chemical)
  Species.left.out <- unique(df$Sp.left.out)
  
  # Save the HC5 values
  hc5.table <- data.frame(i, k, Estimated_HC5, Bootstrapped_HC5, HC5_lower_ci, HC5_upper_ci, b, m, Species.left.out)
  
  # Save each line as a spot in the HC5 list
  individual.hc5s.for.chem[[k]] <- hc5.table
  
  }
  
  # Combine all the elements of the list together (don't overwrite)
  individual.hc5s.for.chem.1 <- rbindlist(individual.hc5s.for.chem)
  
  # Then save each all the runs for a single chemical together as a space in a list
  individual.hc5s[[i]] <- individual.hc5s.for.chem.1
  
}

# Finally, combine data from all chemicals together
individual.hc5s.LOO <- rbindlist(individual.hc5s)

### Check LOO loop output and clean up data
#Quickly check the work we did to see it works

# First check for duplicates (the loop may have ran through an extra time)
individual.hc5s.LOO <- unique(individual.hc5s.LOO)

# Change column names to match the other HC5 output
colnames(individual.hc5s.LOO) <- c("Chemical", "Iteration", "SSD Estimated HC5", "SSD Bootstrapped HC5", "HC5 Lower CI", "HC5 Upper CI", "SSD Dist.", "SSD AD", "Species Left Out")
