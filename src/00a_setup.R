#Alex Dhond's "Analysis_03_Carbamate_Fish"
#Analyze HC5 estimation accuracy for carbamates and fish


# Clear existing workspace
rm(list = ls())

### Load in required packages
library(dplyr) # Used for data manipulation
library(tidyr) # data manipulation
library(RcppAlgos) # Used to create random draws of chemicals
library(data.table) # Used for creating data tables
library(EnvStats) # Used for calculating geometric means
library(plotrix) # used for calculating standard error
library(fitdistrplus) # for fitting various distributions to the data
library(flexsurv) # for fitting parametric distributions
library(ggplot2) # for plotting
library(ggpubr) # for exporting plots

### Load in the source data
#This is WEB-ICE data that has already been filtered to only include chemicals that have data for at least 8 different species

# Read in the WEB ICE data
data <- read.csv("https://raw.githubusercontent.com/glted-mib/SSDn-Code/main/ICE_data_8_species.csv")

### Filter the data to only include carbamate chemicals and fish species
data <- data %>%
  filter(
    Specific_MOA == "Carbamate"
  )

# Filter to only include fish species
data <- data %>%
  filter(
    Taxa == "Fish"
  )

#idk why Alex does this, but he checks that the data only has 5 chemicals
unique(data$Chemical)
