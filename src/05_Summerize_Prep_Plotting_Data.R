#We have added all relevant data and variables to our final dataset, which we can now use to make plots. 

# 5. Summarize and prepare plotting data

### Prepare LOO vs SSDn comparison data
#The steps in this code chunk are pretty bulky, but what I am trying to do here with the wide vs long data is two things. First, by making the data into wide data I can export it cleanly as a table, which can go straight into the manuscript. The long data is needed specifically for plotting.

### Generate the LOO plotting data
LOO.plotting.data.wide <- individual.hc5s.LOO %>%
  dplyr::select(Chemical, Iteration, `SSD Estimated HC5`) %>% # select only these columns
  dplyr::group_by(Chemical) %>% # group by chemical
  mutate(
    LOO.mean.HC5 = mean(`SSD Estimated HC5`), # calculate mean
    LOO.SD.HC5 = sd(`SSD Estimated HC5`), # calculate SD
    LOO.std.err = LOO.SD.HC5 / sqrt(length(Iteration)), # calculate standard error
    LOO.upper.CI = (LOO.mean.HC5 + 1.96*LOO.std.err), # calculate upper CI
    LOO.lower.CI = (LOO.mean.HC5 - 1.96*LOO.std.err), # calculate lower CI
    LOO.SD.upper = (LOO.mean.HC5 + LOO.SD.HC5), # calculate mean + SD
    LOO.SD.lower = (LOO.mean.HC5 - LOO.SD.HC5)) %>% # calculate mean - SD
  ungroup() # ungroup to revert data back to how it was

# Select only the mean/sd/se/ci columns
LOO.plotting.data.wide <- LOO.plotting.data.wide[ ,c(1,4:10)]
# Remove duplicates
LOO.plotting.data.wide <- unique(LOO.plotting.data.wide)

# Now we have a nice summary table of the HC5 data using the LOO method

### Clean the SSDn plotting data
SSDn.plotting.data.wide <- Final.Data %>%
  dplyr::select(
    Chemical, chemical.mean.HC5, chemical.sd.HC5, chemical.std.err.HC5, upper.CI, lower.CI, upper.SD.limit, lower.SD.limit) %>%
  unique()

# Same with the SSDn method

# Combine the two together so we can convert to long format
combined.plot.data <- left_join(LOO.plotting.data.wide, SSDn.plotting.data.wide)

# First extract just the means and convert to long format
long.data.means <- combined.plot.data %>%
  dplyr::select(Chemical, LOO.mean.HC5, chemical.mean.HC5) %>% # select only the chemical and two mean variables
  pivot_longer(cols = c("LOO.mean.HC5", "chemical.mean.HC5"), # pivot longer 
               names_to = "Method",
               values_to = "HC5.Mean.Estimate")
# Rename the method to LOO vs SSDn
long.data.means[long.data.means == "LOO.mean.HC5"] <- "LOO"
long.data.means[long.data.means == "chemical.mean.HC5"] <- "SSDn"

# Now do same for SDs
long.data.SDs <- combined.plot.data %>%
  dplyr::select(Chemical, LOO.SD.HC5, chemical.sd.HC5)%>%
  pivot_longer(cols = c("LOO.SD.HC5", "chemical.sd.HC5"),
               names_to = "Method",
               values_to = "HC5.SD.Estimate")
# Rename the method to LOO vs SSDn
long.data.SDs[long.data.SDs == "LOO.SD.HC5"] <- "LOO"
long.data.SDs[long.data.SDs == "chemical.sd.HC5"] <- "SSDn"

# Now do same for standard errors
long.data.SEs <- combined.plot.data %>%
  dplyr::select(Chemical, LOO.std.err, chemical.std.err.HC5)%>%
  pivot_longer(cols = c("LOO.std.err", "chemical.std.err.HC5"),
               names_to = "Method",
               values_to = "HC5.Std.Err.Estimate")
# Rename the method to LOO vs SSDn
long.data.SEs[long.data.SEs == "LOO.std.err"] <- "LOO"
long.data.SEs[long.data.SEs == "chemical.std.err.HC5"] <- "SSDn"

# Combine together
long.data.total <- left_join(long.data.means, long.data.SDs)
long.data.total <- left_join(long.data.total, long.data.SEs)

# add in upper and lower CIs
long.data.total <- long.data.total %>%
  dplyr::mutate(
    HC5.Upper.CI = HC5.Mean.Estimate + 1.96*HC5.Std.Err.Estimate,
    HC5.Lower.CI = HC5.Mean.Estimate - 1.96*HC5.Std.Err.Estimate)


## Add in single chem SSD upper and lower CI to long data total
#We want to compare the CIs for all three methods. For the conventional SSD method, we are using the upper and lower confidence interval of the best-fit distribution.

single.individual.HC5s
single.ssd.long <- single.individual.HC5s %>%
  dplyr::select(Chemical, `SSD Estimated HC5`, `HC5 Lower CI`, `HC5 Upper CI`)

## add in relevant columns
single.ssd.long$Method <- "SSD"
single.ssd.long$HC5.SD.Estimate <- NA
single.ssd.long$HC5.Std.Err.Estimate <- NA

# re order the columns
single.ssd.long <- single.ssd.long[, c(1,5,2,6,7,4,3)] # leave the row index blank to keep all rows

# rename columns
colnames(single.ssd.long)[3] <- "HC5.Mean.Estimate"
colnames(single.ssd.long)[6] <- "HC5.Upper.CI"
colnames(single.ssd.long)[7] <- "HC5.Lower.CI"

long.data.total <- rbind(long.data.total, single.ssd.long)


### Extract relevant data for manuscript
#I want to compare the output of the single-chemical HC5s and the LOO HC5s.

# Calculate fold difference between them. Fold difference is given as (Y/X - 1) where Y is the final value and X is the starting value

### Aminocarb fold difference
single.individual.HC5s$`SSD Estimated HC5`[single.individual.HC5s$Chemical == "Aminocarb"]/ long.data.total$HC5.Mean.Estimate[long.data.total$Chemical == "Aminocarb" & long.data.total$Method == "LOO"] - 1
long.data.total$HC5.Mean.Estimate[long.data.total$Chemical == "Aminocarb" & long.data.total$Method == "LOO"] / single.individual.HC5s$`SSD Estimated HC5`[single.individual.HC5s$Chemical == "Aminocarb"] - 1

### Carbaryl fold difference
single.individual.HC5s$`SSD Estimated HC5`[single.individual.HC5s$Chemical == "Carbaryl"]/ long.data.total$HC5.Mean.Estimate[long.data.total$Chemical == "Carbaryl" & long.data.total$Method == "LOO"] - 1
long.data.total$HC5.Mean.Estimate[long.data.total$Chemical == "Carbaryl" & long.data.total$Method == "LOO"] / single.individual.HC5s$`SSD Estimated HC5`[single.individual.HC5s$Chemical == "Carbaryl"] - 1

### Carbofuran fold difference
single.individual.HC5s$`SSD Estimated HC5`[single.individual.HC5s$Chemical == "Carbofuran"]/ long.data.total$HC5.Mean.Estimate[long.data.total$Chemical == "Carbofuran" & long.data.total$Method == "LOO"] - 1
long.data.total$HC5.Mean.Estimate[long.data.total$Chemical == "Carbofuran" & long.data.total$Method == "LOO"] / single.individual.HC5s$`SSD Estimated HC5`[single.individual.HC5s$Chemical == "Carbofuran"] - 1

### Methomyl fold difference
single.individual.HC5s$`SSD Estimated HC5`[single.individual.HC5s$Chemical == "Methomyl"]/ long.data.total$HC5.Mean.Estimate[long.data.total$Chemical == "Methomyl" & long.data.total$Method == "LOO"] - 1
long.data.total$HC5.Mean.Estimate[long.data.total$Chemical == "Methomyl" & long.data.total$Method == "LOO"] / single.individual.HC5s$`SSD Estimated HC5`[single.individual.HC5s$Chemical == "Methomyl"] - 1

### Mexacarbate fold difference
single.individual.HC5s$`SSD Estimated HC5`[single.individual.HC5s$Chemical == "Mexacarbate"]/ long.data.total$HC5.Mean.Estimate[long.data.total$Chemical == "Mexacarbate" & long.data.total$Method == "LOO"] - 1
long.data.total$HC5.Mean.Estimate[long.data.total$Chemical == "Mexacarbate" & long.data.total$Method == "LOO"] / single.individual.HC5s$`SSD Estimated HC5`[single.individual.HC5s$Chemical == "Mexacarbate"] - 1
