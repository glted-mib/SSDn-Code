# 2. Build single-chemical SSDs and extract HC5 values

### Extracting HC5 values for single chemicals

# Create a list of chemicals from my data set
single.ssd.chems <- unique(data$Chemical)

# The individual.hc5s list will contain HC5 values from chemical-specific SSDs built using the best-fit method. These HC5s will be compared to the HC5s derived from SSDns.
individual.hc5s <- list()

#This loop calculates HC5 values from chemical-specific SSDs for each chemical in the all.chems list. It uses the best-fit method to fit the most appropriate distribution to individual chemical toxicity data (out of log-normal, log-logisitc, Weibull, and gamma distributions). The Anderson-Darling statistic is used to determine best-fit. The distribution with the lowest Anderson-Darling statistic is assigned as the "best.fit" method.
for(i in single.ssd.chems) {
  
  fit_gamma <- NULL
  best.fit <- NULL
  
  #Calculating the average toxicity value (LC50) for each species 
  IndividualSSDs <- data %>% filter(Chemical == i) %>%
    group_by(Species) %>%
    summarise(
      average = geoMean(Conc), # here I am using "average" instead of "geomean" as earlier
      minimum = min(Conc),
      sd = sd(Conc),
      n = n())
  
  IndividualSSDs <- unique(IndividualSSDs) #Adding toxicity data for each chemical to a list and making sure there are no duplicates
  
  # Create intermediate data frame
  Newdf <- IndividualSSDs
  
  #Calculating the probability points for each species 
  df <- Newdf[order(Newdf$average),]
  df$frac <- ppoints(df$average, 0.5)
  df
  
  #Fit a distribution to the plotted points
  fit_llogis <- fitdist(df$average, "llogis")
  fit_lnorm <- fitdist(df$average, "lnorm")
  fit_gamma <- try(fitdist(df$average, "gamma"), silent = TRUE) # use try here as gamma often fails
  fit_weibull <- fitdist(df$average, "weibull")
  
  # if there is an error with the gamma distribution, fit the other three instead
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
  
  # Save the bootstrap and upper and lower confidence intervals
  Bootstrapped_HC5 <- bootstrap_hc5$quantiles$`p=0.05`
  HC5_lower_ci <- bootstrap_hc5$quantCI[1,1]
  HC5_upper_ci <- bootstrap_hc5$quantCI[2,1]
  
  # Save the HC5 values
  hc5.table <- data.frame(i, Estimated_HC5, Bootstrapped_HC5, HC5_lower_ci, HC5_upper_ci, b, m)
  
  individual.hc5s[[i]] <- hc5.table
}

#Binding the list of individual SSD hc5 values together 
individual.hc5s <- rbindlist(individual.hc5s)
colnames(individual.hc5s) <- c("Chemical", "SSD Estimated HC5", "SSD Bootstrapped HC5", "HC5 Lower CI", "HC5 Upper CI", "SSD Dist.", "SSD AD")

# Save the single-chemical HC5s as their own dataframe
single.individual.HC5s <- individual.hc5s
