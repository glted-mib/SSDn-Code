# 6. Calculate uncertainty and accuracy


#I am going to calculate fold-difference as the primary measure of accuracy (as in fold increase or decrease) and a ratio of the upper confidence interval to the estimate as a measure of uncertainty.

### Create a function for calculating the absolute value of fold-difference

# Create the function. The fold difference gives the absolute value of the maximum of the two numbers being compared. If you want to look for directionality to determine if one value is lower than the other, then also compute the actual difference as chemA/chemB and just remember which one is your numerator and which is your denominator. I always calculate an “actual difference” every time I work with fold difference to ensure there isn’t a bias in over or under-estimation.
fold.diff <- function(prediction, actual) {
  return(max(prediction/actual, actual/prediction))
}

## test
fold.diff(10, 20)
fold.diff(20, 10)

# So what is happening here is we are simply getting the absolute value of the fold difference, which makes sense for our plots. IE going from 20 to 10 is a 2-fold decrease, and going from 10 to 20 is a 2-fold increase. Similarly, going from 5 to 10 is also a 2-fold increase, and 10 to 5 is a 2-fold decrease.


### Add in the single-chemical SSD HC5s to the long-data total dataset

# Select the chemical and the single chemical HC5 columns and save as new dataframe
SSD.HC5s <- single.individual.HC5s %>%
  dplyr::select(Chemical, `SSD Estimated HC5`)

# Join them to the metric data frame
metric.data <- left_join(long.data.total, SSD.HC5s)

# Rename column names
colnames(metric.data)[8] <- "Single.SSD.HC5"

#We developed two metrics (accuracy and uncertainty) to determine what, if any, factors were driving the relatively poor predictions seen in Dichlorvos and Naled in the Organophosphate Invertebrate data set. The accuracy metric we created is a measure of fold-change, and measures how far away the LOO and SSDn-derived HC5 values are from the conventional single-chemical SSD-derived HC5 values. In the accuracy metric, a value of 1 represents the lowest possible value and the most accurate prediction. The further away the accuracy values are from one, the less accurate the prediction is.

### Calculate an accuracy metric based on the fold-difference

# the function does not work for data frames, so we have to apply it using a loop

### Set up a n empty list to hold the accuracy results along with the chemical identifier
accuracy.list <- list()
for (i in 1:nrow(metric.data)){
  Chemical <- metric.data$Chemical[i]
  Method <- metric.data$Method[i]
  Accuracy <- fold.diff(metric.data$HC5.Mean.Estimate[i], metric.data$Single.SSD.HC5[i])
  accuracy.list[[i]] <- data.table(Chemical, Method, Accuracy)
}

# Make it into a data frame
accuracy.list <- rbindlist(accuracy.list)

# Join into the accuracy test data frame
metric.data <- left_join(metric.data, accuracy.list)


### Uncertainty metric
#The uncertainty metric we developed is the ratio of the SSDn upper CI to the SSDn HC5 average, given as A/B where A is the SSDn upper CI and B is the SSDn HC5 average.
#In this metric, the closer you get to 1, the more certain you are. And the further away you get from 1, the more uncertain you are.

### Create a test data frame with the metric
metric.data <- metric.data %>% # take from metric data
  dplyr::mutate(
    Uncertainty = HC5.Upper.CI/HC5.Mean.Estimate) # create the metric

# We have the metric, now we need to add in relevant factors that we think might be affecting uncertainty (so we can run a linear model/regression)

### Add in number of species in original single chemical SSD

# Filter and summarize the data to see which chemicals have the most species
no.spec.in.SSD.data <- data %>% 
  dplyr::group_by(Chemical) %>% # group by chemical
  summarise(
    number_species = n_distinct(Species)) %>% # calculate number of species
  arrange(desc(number_species)) # arrange in descending order (most species at top)

### join to uncertainty test data
metric.data <- left_join(metric.data, no.spec.in.SSD.data)

### Rename column name
colnames(metric.data)[11] <- "Number.Species.in.SSD"


# Now add in the number of nSpecies (take this from the final data)
### add in number of nSpecies
no.n.spec <- Final.Data %>%
  dplyr::group_by(Chemical) %>%
  summarise(
    no.n.species = n_distinct(nSpecies)) %>% # calculate number of species
  arrange(desc(no.n.species)) # arrange in descending order (most species at top)

### Left join this into the uncertainty data
metric.data <- left_join(metric.data, no.n.spec)

### Rename column name
colnames(metric.data)[12] <- "Number.nSpecies.in.SSDn"


### Add in the range of min-max tox values from single-chemical SSD
# Range in min-max tox values from single-chemical SSD
ssd.range <- data %>%
  dplyr::group_by(Chemical) %>%
  summarise(
    Chemical = Chemical,
    min.SSD.tox.val = min(Conc), # take min tox value
    max.SSD.tox.val = max(Conc), # take max tox value 
    min.max.SSD.range = max.SSD.tox.val - min.SSD.tox.val # take difference between them
  )

# Take only unique values
ssd.range <- unique(ssd.range)

# Join to metric data set
metric.data <- left_join(metric.data, ssd.range)


### Export the metric data as a CVS file so it can be used to create a table

write.csv(metric.data, "carbamate_fish_metric_data.csv")
