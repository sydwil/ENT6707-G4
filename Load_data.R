### Load Libraries ############################################################
library(tidyr)
library(dplyr)
library(ggplot2)


### Read in the file ##########################################################
carnivore <- read.csv(file = "G4_hf111-01-prey.csv", header = TRUE, 
                      na.strings = "NA")

str(carnivore)
head(carnivore)

carnivore$species
