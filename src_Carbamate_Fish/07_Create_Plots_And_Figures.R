# 7. Create plots and figures 

### Update the factor levels for plots
 
# make method a factor with these levels to match the plot
long.data.total$Method <- factor(long.data.total$Method, levels = c("SSD", "LOO", "SSDn"))

# manually change the factor levels of the chemicals so that it goes in order from smallest HC5 to largest HC5
long.data.total$Chemical <- factor(long.data.total$Chemical, levels = c("Carbofuran", "Methomyl", "Carbaryl","Aminocarb", "Mexacarbate"))



### Plot 1: Comparison plot including the single-chemical HC5 as well
#This plot has three values, the LOO HC5 mean and CI, the single-chemical HC5, and the SSDn HC5 mean and CI.

# first make long data a factor so it can be plotted
test$Method <- as.factor(test$Method)

# set up color palette
comp.plot.colours <- c(SSD = "#000000", LOO = "#000000", SSDn = "#000000") # color palette (all black)
comp.plot.shapes <- c(SSD = 17, LOO = 19, SSDn = 15) # shape types

# Create a comparison plot
comparison.plot <- test %>%
  ggplot()+
  aes(x = Chemical, y = HC5.Mean.Estimate, ymin = HC5.Lower.CI, ymax = HC5.Upper.CI, color = Method, shape = Method)+
  geom_point(size = 2.5, position = position_dodge(width = 0.75))+ # add in the points, dodge so they don't hit or overlap each other
  geom_linerange(aes(ymax= HC5.Upper.CI, ymin = pmax(HC5.Lower.CI, 0)), size = 1, position = position_dodge(width = 0.75))+ # set line ranges as the upper and lower CIs
  geom_vline(xintercept = 1.5)+ # add in vertical line to separate chemicals
  geom_vline(xintercept = 2.5)+ # add in vertical line to separate chemicals
  geom_vline(xintercept = 3.5)+ # add in vertical line to separate chemicals
  geom_vline(xintercept = 4.5)+ # add in vertical line to separate chemicals
  geom_segment(aes(x = 0.5, xend = 1.5, 
                   y = (unique(Final.Data$Single.C.HC5.bf[Final.Data$Chemical == "Carbofuran"]*5)),
                   yend = unique((Final.Data$Single.C.HC5.bf[Final.Data$Chemical == "Carbofuran"]*5))),
               colour = "black", linetype = "dashed")+ # add in 5 fold upper for Carbofuran (from true single-chem HC5)
  geom_segment(aes(x = 0.5, xend = 1.5, 
                   y = (unique(Final.Data$Single.C.HC5.bf[Final.Data$Chemical == "Carbofuran"]/5)), 
                   yend = (unique(Final.Data$Single.C.HC5.bf[Final.Data$Chemical == "Carbofuran"]/5))),
               colour = "black", linetype = "dashed")+ # add in 5 fold lower for Carbofuran
  geom_segment(aes(x = 0.5, xend = 1.5, 
                   y = (unique(Final.Data$Single.C.HC5.bf[Final.Data$Chemical == "Carbofuran"]*3)), 
                   yend = (unique(Final.Data$Single.C.HC5.bf[Final.Data$Chemical == "Carbofuran"]*3))),
               colour = "black", linetype = "dotted")+ # add in 3 fold upper for Carbofuran
  geom_segment(aes(x = 0.5, xend = 1.5, 
                   y = (unique(Final.Data$Single.C.HC5.bf[Final.Data$Chemical == "Carbofuran"]/3)), 
                   yend = (unique(Final.Data$Single.C.HC5.bf[Final.Data$Chemical == "Carbofuran"]/3))),
               colour = "black", linetype = "dotted")+ # add in 3 fold lower for Carbofuran
  geom_segment(aes(x = 1.5, xend = 2.5, 
                   y = (unique(Final.Data$Single.C.HC5.bf[Final.Data$Chemical == "Methomyl"]*5)), 
                   yend = (unique(Final.Data$Single.C.HC5.bf[Final.Data$Chemical == "Methomyl"]*5))),
               colour = "black", linetype = "dashed")+ # add in 5 fold upper for Methomyl
  geom_segment(aes(x = 1.5, xend = 2.5, 
                   y = (unique(Final.Data$Single.C.HC5.bf[Final.Data$Chemical == "Methomyl"]/5)), 
                   yend = (unique(Final.Data$Single.C.HC5.bf[Final.Data$Chemical == "Methomyl"]/5))),
               colour = "black", linetype = "dashed")+ # add in 5 fold lower for Methomyl
  geom_segment(aes(x = 1.5, xend = 2.5, 
                   y = (unique(Final.Data$Single.C.HC5.bf[Final.Data$Chemical == "Methomyl"]*3)), 
                   yend = (unique(Final.Data$Single.C.HC5.bf[Final.Data$Chemical == "Methomyl"]*3))),
               colour = "black", linetype = "dotted")+ # add in 3 fold upper for Methomyl
  geom_segment(aes(x = 1.5, xend = 2.5, 
                   y = (unique(Final.Data$Single.C.HC5.bf[Final.Data$Chemical == "Methomyl"]/3)), 
                   yend = (unique(Final.Data$Single.C.HC5.bf[Final.Data$Chemical == "Methomyl"]/3))),
               colour = "black", linetype = "dotted")+ # add in 3 fold lower for Methomyl
  geom_segment(aes(x = 2.5, xend = 3.5, 
                   y = (unique(Final.Data$Single.C.HC5.bf[Final.Data$Chemical == "Carbaryl"]*5)), 
                   yend = (unique(Final.Data$Single.C.HC5.bf[Final.Data$Chemical == "Carbaryl"]*5))),
               colour = "black", linetype = "dashed")+ # add in 5 fold upper for Carbaryl
  geom_segment(aes(x = 2.5, xend = 3.5, 
                   y = (unique(Final.Data$Single.C.HC5.bf[Final.Data$Chemical == "Carbaryl"]/5)), 
                   yend = (unique(Final.Data$Single.C.HC5.bf[Final.Data$Chemical == "Carbaryl"]/5))),
               colour = "black", linetype = "dashed")+ # add in 5 fold lower for Carbaryl
  geom_segment(aes(x = 2.5, xend = 3.5, 
                   y = (unique(Final.Data$Single.C.HC5.bf[Final.Data$Chemical == "Carbaryl"]*3)), 
                   yend = (unique(Final.Data$Single.C.HC5.bf[Final.Data$Chemical == "Carbaryl"]*3))),
               colour = "black", linetype = "dotted")+ # add in 3 fold upper for Carbaryl
  geom_segment(aes(x = 2.5, xend = 3.5, 
                   y = (unique(Final.Data$Single.C.HC5.bf[Final.Data$Chemical == "Carbaryl"]/3)), 
                   yend = (unique(Final.Data$Single.C.HC5.bf[Final.Data$Chemical == "Carbaryl"]/3))),
               colour = "black", linetype = "dotted")+ # add in 3 fold upper for Carbaryl
  geom_segment(aes(x = 3.5, xend = 4.5, 
                   y = (unique(Final.Data$Single.C.HC5.bf[Final.Data$Chemical == "Aminocarb"]*5)), 
                   yend = (unique(Final.Data$Single.C.HC5.bf[Final.Data$Chemical == "Aminocarb"]*5))),
               colour = "black", linetype = "dashed")+ # add in 5 fold upper for Aminocarb
  geom_segment(aes(x = 3.5, xend = 4.5, 
                   y = (unique(Final.Data$Single.C.HC5.bf[Final.Data$Chemical == "Aminocarb"]/5)), 
                   yend = (unique(Final.Data$Single.C.HC5.bf[Final.Data$Chemical == "Aminocarb"]/5))),
               colour = "black", linetype = "dashed")+ # add in 5 fold lower for Aminocarb
  geom_segment(aes(x = 3.5, xend = 4.5, 
                   y = (unique(Final.Data$Single.C.HC5.bf[Final.Data$Chemical == "Aminocarb"]*3)), 
                   yend = (unique(Final.Data$Single.C.HC5.bf[Final.Data$Chemical == "Aminocarb"]*3))),
               colour = "black", linetype = "dotted")+ # add in 3 fold upper for Aminocarb
  geom_segment(aes(x = 3.5, xend = 4.5, 
                   y = (unique(Final.Data$Single.C.HC5.bf[Final.Data$Chemical == "Aminocarb"]/3)), 
                   yend = (unique(Final.Data$Single.C.HC5.bf[Final.Data$Chemical == "Aminocarb"]/3))),
               colour = "black", linetype = "dotted")+ # add in 3 fold lower for Aminocarb
  geom_segment(aes(x = 4.5, xend = 5.5, 
                   y = (unique(Final.Data$Single.C.HC5.bf[Final.Data$Chemical == "Mexacarbate"]*5)), 
                   yend = (unique(Final.Data$Single.C.HC5.bf[Final.Data$Chemical == "Mexacarbate"]*5))),
               colour = "black", linetype = "dashed")+ # add in 5 fold upper for Mexacarbate
  geom_segment(aes(x = 4.5, xend = 5.5, 
                   y = (unique(Final.Data$Single.C.HC5.bf[Final.Data$Chemical == "Mexacarbate"]/5)), 
                   yend = (unique(Final.Data$Single.C.HC5.bf[Final.Data$Chemical == "Mexacarbate"]/5))),
               colour = "black", linetype = "dashed")+ # add in 5 fold lower for Mexacarbate
  geom_segment(aes(x = 4.5, xend = 5.5, 
                   y = (unique(Final.Data$Single.C.HC5.bf[Final.Data$Chemical == "Mexacarbate"]*3)), 
                   yend = (unique(Final.Data$Single.C.HC5.bf[Final.Data$Chemical == "Mexacarbate"]*3))),
               colour = "black", linetype = "dotted")+ # add in 3 fold upper for Mexacarbate
  geom_segment(aes(x = 4.5, xend = 5.5, 
                   y = (unique(Final.Data$Single.C.HC5.bf[Final.Data$Chemical == "Mexacarbate"]/3)), 
                   yend = (unique(Final.Data$Single.C.HC5.bf[Final.Data$Chemical == "Mexacarbate"]/3))),
               colour = "black", linetype = "dotted")+ # add in 3 fold lower for Mexacarbate
  theme_bw()+ # black and white theme
  ylab(expression(bold(paste("HC5 (", mu, "g/L)"))))+ # add y axis label
  scale_y_log10()+ # make it a log scale
  scale_shape_manual(values = comp.plot.shapes)+ # set shapes to the palette I chose
  scale_color_manual(values = comp.plot.colours)+ # set colours to the colour palette I chose
  scale_x_discrete(expand = c(0, 0.5))+ # cut off the left and right edge of the graph so all boxes are same size
  theme(axis.text.x = element_text(face = "bold", angle = 45, hjust = 1, size = 13, colour = "black"), # rotate labels 45 degrees for reading, make font 11 point
        axis.title.x = element_blank(), # remove x axis label
        axis.title.y = element_text(face = "bold", size = 14),
        axis.text.y = element_text(face = "bold", size = 13, colour = "black"),
        legend.text = element_text(face = "bold", size = 13, colour = "black"),
        legend.title = element_text(face = "bold", size = 13))
comparison.plot


## Individual Chemical Plotting

### Chemical 1: Aminocarb

#### Plot 1: Raw SSDn Output plot
#The chunk below filters the data only for Aminocarb, then plots the raw output of all the SSDn results. The plot shows that overall, most nSpecies under-estimated the HC5 compared to the "true" HC5.

# Filter the data to only include aminocarb
Aminocarb <- Final.Data %>%
  filter(
    Chemical == "Aminocarb"
  )

# Arrange the nSpecies by the toxicity value (so the most sensitive species are at the top)
Aminocarb <- Aminocarb %>%
   arrange(average, nSpecies)

# Extract the order of the species and check
Aminocarb.n.spec <- unique(Aminocarb$nSpecies)
Aminocarb.n.spec

# Relevel the factors to match the order
Aminocarb$nSpecies <- factor(Aminocarb$nSpecies, levels = Aminocarb.n.spec)
       
# Create the plot
Aminocarb.plot <- ggplot(Aminocarb, aes(x = nSpecies, y = bf.HC5))+ # X axis is the nSpecies, Y axis is the back-calculated HC5s that were generated from SSDs that used the nSpecies
  geom_point(position = position_dodge(width = 0.5))+ # add in the points
  geom_hline(yintercept = Aminocarb$Single.C.HC5.bf, colour = "red")+ # Add the "true" HC5 as a red line across
  geom_hline(yintercept = (Aminocarb$Single.C.HC5.bf*5), colour = "red", linetype = 2)+ # Add the 5-fold higher mark as a dashed red line
  geom_hline(yintercept = (Aminocarb$Single.C.HC5.bf/5), colour = "red", linetype = 2)+ # Add the 5-fold lower mark as a dashed red line
  geom_vline(xintercept = 3.5, linetype = 2)+ # Designate the first quartile to the left of the line
  theme_bw()+ # black and white theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ # rotate x axis labels 45 degrees for reading
  scale_y_log10()+ # make whole graph log scale
  xlab("nSpecies")+
  ylab("HC5 (ug/L)")+
  ggtitle("Aminocarb SSDn Estimated HC5s (FISH ONLY)") # add title
Aminocarb.plot




### Chemical 2: Carbaryl

#### Plot 1: Raw SSDn Output plot
#The chunk below filters the data only for Carbaryl, then plots the raw output of all the SSDn results. The plot shows that overall, most nSpecies under-estimated the HC5 compared to the "true" HC5.

# Filter the data to only include Carbaryl
Carbaryl <- Final.Data %>%
  filter(
    Chemical == "Carbaryl"
  )

# Arrange the nSpecies by the toxicity value (so the most sensitive species are at the top)
Carbaryl <- Carbaryl %>%
   arrange(average, nSpecies)

# Extract the order of the species and check
Carbaryl.n.spec <- unique(Carbaryl$nSpecies)
Carbaryl.n.spec

# Relevel the factors to match the order
Carbaryl$nSpecies <- factor(Carbaryl$nSpecies, levels = Carbaryl.n.spec)
       
# Create the plot
Carbaryl.plot <- ggplot(Carbaryl, aes(x = nSpecies, y = bf.HC5))+ # X axis is the nSpecies, Y axis is the back-calculated HC5s that were generated from SSDs that used the nSpecies
  geom_point(position = position_dodge(width = 0.5))+ # add in the points
  geom_hline(yintercept = Carbaryl$Single.C.HC5.bf, colour = "red")+ # Add the "true" HC5 as a red line across
  geom_hline(yintercept = (Carbaryl$Single.C.HC5.bf*5), colour = "red", linetype = 2)+ # Add the 5-fold higher mark as a dashed red line
  geom_hline(yintercept = (Carbaryl$Single.C.HC5.bf/5), colour = "red", linetype = 2)+ # Add the 5-fold lower mark as a dashed red line
  geom_vline(xintercept = 3.5, linetype = 2)+ # Designate the first quartile to the left of the line
  theme_bw()+ # black and white theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ # rotate x axis labels 45 degrees for reading
  scale_y_log10()+ # make whole graph log scale
  xlab("nSpecies")+
  ylab("HC5 (ug/L)")+
  ggtitle("Carbaryl SSDn Estimated HC5s (FISH ONLY)") # add title
Carbaryl.plot


### Chemical 3: Carbofuran

#### Plot 1: Raw SSDn Output plot
#The chunk below filters the data only for Carbofuran, then plots the raw output of all the SSDn results. The plot shows that overall, most nSpecies under-estimated the HC5 compared to the "true" HC5.

# Filter the data to only include Carbofuran
Carbofuran <- Final.Data %>%
  filter(
    Chemical == "Carbofuran"
  )

# Arrange the nSpecies by the toxicity value (so the most sensitive species are at the top)
Carbofuran <- Carbofuran %>%
   arrange(average, nSpecies)

# Extract the order of the species and check
Carbofuran.n.spec <- unique(Carbofuran$nSpecies)
Carbofuran.n.spec

# Relevel the factors to match the order
Carbofuran$nSpecies <- factor(Carbofuran$nSpecies, levels = Carbofuran.n.spec)
       
# Create the plot
Carbofuran.plot <- ggplot(Carbofuran, aes(x = nSpecies, y = bf.HC5))+ # X axis is the nSpecies, Y axis is the back-calculated HC5s that were generated from SSDs that used the nSpecies
  geom_point(position = position_dodge(width = 0.5))+ # add in the points
  geom_hline(yintercept = Carbofuran$Single.C.HC5.bf, colour = "red")+ # Add the "true" HC5 as a red line across
  geom_hline(yintercept = (Carbofuran$Single.C.HC5.bf*5), colour = "red", linetype = 2)+ # Add the 5-fold higher mark as a dashed red line
  geom_hline(yintercept = (Carbofuran$Single.C.HC5.bf/5), colour = "red", linetype = 2)+ # Add the 5-fold lower mark as a dashed red line
  geom_vline(xintercept = 3.5, linetype = 2)+ # Designate the first quartile to the left of the line
  theme_bw()+ # black and white theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ # rotate x axis labels 45 degrees for reading
  theme(legend.title = element_text("nSpecies Taxa"))+
  labs(color = "nSpecies Taxa")+
  scale_y_log10()+ # make whole graph log scale
  xlab("nSpecies")+
  ylab("HC5 (ug/L)")+
  ggtitle("Carbofuran SSDn Estimated HC5s (FISH ONLY)") # add title
Carbofuran.plot



### Chemical 4: Methomyl

#### Plot 1: Raw SSDn Output plot
#The chunk below filters the data only for Methomyl, then plots the raw output of all the SSDn results. The plot shows that overall, most nSpecies under-estimated the HC5 compared to the "true" HC5.

# Filter the data to only include Methomyl
Methomyl <- Final.Data %>%
  filter(
    Chemical == "Methomyl"
  )

# Arrange the nSpecies by the toxicity value (so the most sensitive species are at the top)
Methomyl <- Methomyl %>%
   arrange(average, nSpecies)

# Extract the order of the species and check
Methomyl.n.spec <- unique(Methomyl$nSpecies)
Methomyl.n.spec

# Relevel the factors to match the order
Methomyl$nSpecies <- factor(Methomyl$nSpecies, levels = Methomyl.n.spec)
       
# Create the plot
Methomyl.plot <- ggplot(Methomyl, aes(x = nSpecies, y = bf.HC5))+ # X axis is the nSpecies, Y axis is the back-calculated HC5s that were generated from SSDs that used the nSpecies
  geom_point(position = position_dodge(width = 0.5))+ # add in the points
  geom_hline(yintercept = Methomyl$Single.C.HC5.bf, colour = "red")+ # Add the "true" HC5 as a red line across
  geom_hline(yintercept = (Methomyl$Single.C.HC5.bf*5), colour = "red", linetype = 2)+ # Add the 5-fold higher mark as a dashed red line
  geom_hline(yintercept = (Methomyl$Single.C.HC5.bf/5), colour = "red", linetype = 2)+ # Add the 5-fold lower mark as a dashed red line
  geom_vline(xintercept = 3.5, linetype = 2)+ # Designate the first quartile to the left of the line
  theme_bw()+ # black and white theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ # rotate x axis labels 45 degrees for reading
  scale_y_log10()+ # make whole graph log scale
  xlab("nSpecies")+
  ylab("HC5 (ug/L)")+
  ggtitle("Methomyl SSDn Estimated HC5s (FISH ONLY)") # add title
Methomyl.plot


### Chemical 5: Mexacarbate

#### Plot 1: Raw SSDn Output plot
#The chunk below filters the data only for Mexacarbate, then plots the raw output of all the SSDn results. The plot shows that overall, most nSpecies under-estimated the HC5 compared to the "true" HC5.

# Filter the data to only include Mexacarbate
Mexacarbate <- Final.Data %>%
  filter(
    Chemical == "Mexacarbate"
  )

# Arrange the nSpecies by the toxicity value (so the most sensitive species are at the top)
Mexacarbate <- Mexacarbate %>%
   arrange(average, nSpecies)

# Extract the order of the species and check
Mexacarbate.n.spec <- unique(Mexacarbate$nSpecies)
Mexacarbate.n.spec

# Relevel the factors to match the order
Mexacarbate$nSpecies <- factor(Mexacarbate$nSpecies, levels = Mexacarbate.n.spec)
       
# Create the plot
Mexacarbate.plot <- ggplot(Mexacarbate, aes(x = nSpecies, y = bf.HC5))+ # X axis is the nSpecies, Y axis is the back-calculated HC5s that were generated from SSDs that used the nSpecies
  geom_point(position = position_dodge(width = 0.5))+ # add in the points
  geom_hline(yintercept = Mexacarbate$Single.C.HC5.bf, colour = "red")+ # Add the "true" HC5 as a red line across
  geom_hline(yintercept = (Mexacarbate$Single.C.HC5.bf*5), colour = "red", linetype = 2)+ # Add the 5-fold higher mark as a dashed red line
  geom_hline(yintercept = (Mexacarbate$Single.C.HC5.bf/5), colour = "red", linetype = 2)+ # Add the 5-fold lower mark as a dashed red line
  geom_vline(xintercept = 3.5, linetype = 2)+ # Designate the first quartile to the left of the line
  theme_bw()+ # black and white theme
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+ # rotate x axis labels 45 degrees for reading
  scale_y_log10()+ # make whole graph log scale
  xlab("nSpecies")+
  ylab("HC5 (ug/L)")+
  ggtitle("Mexacarbate SSDn Estimated HC5s (FISH ONLY)") # add title
Mexacarbate.plot


# Save the results plot for the manuscript

# save results plot for manuscript as a tiff file
tiff("Carbamate_Fish_Results.tiff", units = "in", width = 7.5, height = 7, res = 300) # load an empty tiff file with these dimensions
comparison.plot # put in the final results plot
dev.off() # close graphics so it saves



# Save and export the SI plots for use in manuscript

### Export all plots to one page of PDF
#Put each plot on its own page in a PDF document

multi.page.plots <- ggarrange(Aminocarb.plot, Carbaryl.plot, Carbofuran.plot, Methomyl.plot, Mexacarbate.plot, nrow=1, ncol=1) # for one plot per page

multi.page.plots
ggexport(multi.page.plots, filename="Carbamate_Fish_SI_Plots.pdf")
