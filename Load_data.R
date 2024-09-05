### Load Libraries ############################################################
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)


### Read in the file ##########################################################
carnivore <- read.csv(file = "G4_hf111-01-prey.csv", header = TRUE, 
                      na.strings = "NA")

unique_species <- unique(carnivore$species) 
  # 49 unique species represented


# first replace spaces in spaces in species names with underscore
carnivore <- carnivore %>% mutate(species = str_replace(species, " ", "_"))
head(carnivore)

# determine number of genera represented
unique_genera <- unique(gsub("_.*","",  x = carnivore$species))
      # this code is funky - apparently the .* means to neglect everything after
      # the pattern you're looking for. In this case, I'm telling it to look
      # for an underscore in each species name, and then remove everything after
      # that, which leaves us with just our genera!

#let's also just add those genera as their own column to the dataframe so that
# we can group by that column later on
carnivore$genus <- gsub("_.*","",  x = carnivore$species)
head(carnivore)
tail(carnivore)


str(carnivore)
head(carnivore)

carnivore$species
