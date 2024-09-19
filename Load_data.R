### Load Libraries ############################################################
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)


### Read in the file ##########################################################
carnivore <- read.csv(file = "G4_hf111-01-prey.csv", header = TRUE, 
                      na.strings = "NA")

# first replace spaces in spaces in species names with underscore
carnivore <- carnivore %>% mutate(species = str_replace(species, " ", "_"))
head(carnivore)
str(carnivore)

unique_species <- unique(carnivore$species) 
# 49 unique species represented




#let's also just add those genera as their own column to the dataframe so that
# we can group by that column later on
carnivore$genus <- gsub("_.*","",  x = carnivore$species)
carnivore$genus

# determine number of genera represented
unique_genera <- unique(gsub("_.*","",  x = carnivore$species))
# this code is funky - apparently the .* means to neglect everything after
# the pattern you're looking for. In this case, I'm telling it to look
# for an underscore in each species name, and then remove everything after
# that, which leaves us with just our genera!


# "Nepenthese" and "Neentes" are both misspellings of the same genus. Can't
# find any plant with those genera but can pull up the same species name with 
# the Nepenthes genus
# "Saracenia" is also evidently a typo! 
# fix the typos!

carnivore <- carnivore %>% 
  mutate(genus = replace(genus, genus == "Nepenthese", "Nepenthes"))
carnivore <- carnivore %>% 
  mutate(genus = replace(genus, genus == "Neentes", "Nepenthes"))
carnivore <- carnivore %>% 
  mutate(genus = replace(genus, genus == "Saracenia", "Sarracenia"))

unique(carnivore$genus)
str(carnivore)




### Add Predation Method column ################################################
carnivore$predation_method <- NA

carnivore <- carnivore %>% 
  mutate(predation_method = replace(predation_method, 
                                    genus == "Pinguicula" |
                                    genus == "Drosera", 
                                    "sticky_traps"))

carnivore <- carnivore %>% 
  mutate(predation_method = replace(predation_method, 
                                    genus == "Nepenthes" |
                                    genus == "Brocchinia" |
                                    genus == "Sarracenia" |
                                    genus == "Triphyophyllum", 
                                    "pitchers"))

carnivore <- carnivore %>% 
  mutate(predation_method = replace(predation_method, 
                                    genus == "Utricularia" |
                                      genus == "Dionaea", 
                                    "active_trapping"))

carnivore$predation_method


#I'll leave this code of this "for-loop" here, but I've discovered using
# the which function works just fine
### i <- 4
### for(i in 1:nrow(carnivore)){
###   ifelse(test = carnivore$genus[i] == "Pinguicula" | carnivore$genus[i] == "Drosera",
###         yes = carnivore$predation_method[i] <- "sticky_traps",
###          no = carnivore$predation[i] <- NA)
### 
### }

#"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

str(carnivore)
head(carnivore)

carnivore$species

# look at units to figure out how we might subdivide them
carnivore$units
carnivore$study
summary(carnivore)

#look at unit types to figure out how best to get units to talk to each
#other.
nrow(filter(carnivore, units == "percentage")) # 30 studies use percentage
nrow(filter(carnivore, units == "number_of_individuals")) # 42 use no. indivs.
nrow(filter(carnivore, units == "mean_number_per_pitcher")) # 10 use this
nrow(filter(carnivore, 
            units != "mean_number_per_pitcher" &
            units != "percentage" &
            units != "number_of_individuals"))

    # Only six studies don't use either percentages, number_of_individuals, or
    # mean number per pitcher for counts of pray

# Take out the ones with weird units
carnivore <- carnivore %>% filter(units == "percentage" |
                                    units == "number_of_individuals" |
                                    units == "mean_number_per_pitcher")
carnivore$study

### Get Percentages ###########################################################
# what does "percentage" mean? percentage of what?
carnivore_percentage <- filter(carnivore, units == "percentage")

carnivore_not_percentage <- carnivore %>% filter(units != "percentage")
carnivore_not_percentage$study

# make new colummns of NA to later fill in with new calculated percentages
carnivore$sum_arthropods <- NA

# This is useless because it's unclear how the percentage ones are calculated,
# so we can't make a true comparison between them
#i <- 1
# for(i in 1:nrow(carnivore)){
# 
# carnivore$sum_arthropods[i] <- ifelse(
#   test = carnivore$units[i] == "number_of_individuals",
#   yes = carnivore$sum_arthropods[i] <- 
#     sum(carnivore$diptera.sum, carnivore$acarina, carnivore$collembola,
#         carnivore$hymenoptera.not.formicidae, carnivore$thysanoptera,
#         carnivore$homoptera.sum, carnivore$coleoptera, carnivore$araneae,
#         carnivore$lepidoptera, carnivore$hemiptera, carnivore$plecoptera,
#         carnivore$formicidae, carnivore$orthoptera, carnivore$protura,
#         carnivore$chilopoda, carnivore$trichoptera, carnivore$isopoda,
#         carnivore$neuroptera, carnivore$diplopoda, carnivore$odonata,
#         carnivore$psocoptera, carnivore$isoptera, carnivore$copeopoda,
#         carnivore$cladocera, carnivore$scorpions, carnivore$cyclopoida,
#         carnivore$harpaticoida),
#   
#   
#   no = carnivore$sum_arthropods[i] <- NA)
# 
# # }



### Look at individual studies ################################################

#Zamora 1990___________________________________________________________________
carnivore_Zamora <- carnivore_not_percentage %>% filter(study == "Zamora 1990")
carnivore_Zamora
#""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

#Karlsson et al. 1994_________________________________________________________
carnivore_Karlsson <- carnivore_not_percentage %>% filter(
  study == "Karlsson et al. 1994")
carnivore_Karlsson
#"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

#Newell & Natase 1998_________________________________________________________
carnivore_NewellNastase <- carnivore_not_percentage %>% filter(
  study == "Newell & Nastase 1998")
carnivore_NewellNastase

#Williams 1980________________________________________________________________
filter(carnivore, study == "Williams 1980")


###Analzye % Studies and Sum Studies Separately ###############################
# for each of these, "n" is the number of rows in the original data frame that 
# were of that genus, in other words, the number of observations on that genus
#### Percentage Data ##########################################################
carnivore_percentage <- filter(carnivore, units == "percentage")
carnivore_percentage$study

str(carnivore_percentage)

hist(carnivore_percentage$diptera.sum)
hist(carnivore_percentage$acarina)
hist(carnivore_percentage$hymenoptera.not.formicidae)
hist(carnivore_percentage$thysanoptera)
hist(carnivore_percentage$homoptera.sum)
hist(carnivore_percentage$coleoptera)
hist(carnivore_percentage$araneae)



dotchart(carnivore_percentage$diptera.sum)

carnivore_percentage_summary <- carnivore_percentage %>% group_by(genus) %>%
  summarise(mean_diptera =mean(diptera.sum),
            mean_acarina = mean(acarina),
            mean_hymenoptera.not.formicidae = mean(hymenoptera.not.formicidae),
            mean_thysanoptera = mean(thysanoptera),
            mean_homoptera.sum = mean(homoptera.sum),
            mean_coleoptera = mean(coleoptera),
            mean_araneae = mean(araneae),
            mean_lepidoptera = mean(lepidoptera),
            mean_hemiptera = mean(hemiptera),
            mean_plecoptera = mean(plecoptera),
            mean_formicidae = mean(formicidae),
            mean_orthoptera = mean(orthoptera),
            mean_protura = mean(protura),
            mean_chilopoda = mean(chilopoda),
            mean_trichoptera = mean(trichoptera),
            mean_isopoda = mean(isopoda),
            mean_neuroptera = mean(neuroptera),
            mean_diplopoda = mean(diplopoda),
            mean_odonata = mean(odonata),
            mean_psocoptera = mean(psocoptera),
            mean_isoptera = mean(isoptera),
            mean_copeopoda = mean(copeopoda),
            mean_cladocera = mean(cladocera),
            mean_scorpions = mean(scorpions),
            mean_cyclopoida = mean(cyclopoida),
            mean_harpaticoida = mean(harpaticoida),
            mean_microptera = mean(microptera),
            n_genus = n())

#repeat but by predation type
carnivore_percentage_summary_pred <- carnivore_percentage %>% group_by(predation_method) %>%
  summarise(mean_diptera =mean(diptera.sum),
            mean_acarina = mean(acarina),
            mean_hymenoptera.not.formicidae = mean(hymenoptera.not.formicidae),
            mean_thysanoptera = mean(thysanoptera),
            mean_homoptera.sum = mean(homoptera.sum),
            mean_coleoptera = mean(coleoptera),
            mean_araneae = mean(araneae),
            mean_lepidoptera = mean(lepidoptera),
            mean_hemiptera = mean(hemiptera),
            mean_plecoptera = mean(plecoptera),
            mean_formicidae = mean(formicidae),
            mean_orthoptera = mean(orthoptera),
            mean_protura = mean(protura),
            mean_chilopoda = mean(chilopoda),
            mean_trichoptera = mean(trichoptera),
            mean_isopoda = mean(isopoda),
            mean_neuroptera = mean(neuroptera),
            mean_diplopoda = mean(diplopoda),
            mean_odonata = mean(odonata),
            mean_psocoptera = mean(psocoptera),
            mean_isoptera = mean(isoptera),
            mean_copeopoda = mean(copeopoda),
            mean_cladocera = mean(cladocera),
            mean_scorpions = mean(scorpions),
            mean_cyclopoida = mean(cyclopoida),
            mean_harpaticoida = mean(harpaticoida),
            mean_microptera = mean(microptera),
            n_predation_method = n())




#### Individual Counts Data ###################################################
carnivore_number_indivs <- filter(carnivore, units == "number_of_individuals")
str(carnivore_number_indivs)

hist(carnivore_number_indivs$diptera.sum)
hist(carnivore_number_indivs$acarina)
hist(carnivore_number_indivs$hymenoptera.not.formicidae)
hist(carnivore_number_indivs$thysanoptera)
hist(carnivore_number_indivs$homoptera.sum)
hist(carnivore_number_indivs$coleoptera)
hist(carnivore_number_indivs$araneae)


carnivore_number_indivs_summary <- carnivore_number_indivs %>% group_by(genus) %>%
  summarise(mean_diptera =mean(diptera.sum),
            mean_acarina = mean(acarina),
            mean_hymenoptera.not.formicidae = mean(hymenoptera.not.formicidae),
            mean_thysanoptera = mean(thysanoptera),
            mean_homoptera.sum = mean(homoptera.sum),
            mean_coleoptera = mean(coleoptera),
            mean_araneae = mean(araneae),
            mean_lepidoptera = mean(lepidoptera),
            mean_hemiptera = mean(hemiptera),
            mean_plecoptera = mean(plecoptera),
            mean_formicidae = mean(formicidae),
            mean_orthoptera = mean(orthoptera),
            mean_protura = mean(protura),
            mean_chilopoda = mean(chilopoda),
            mean_trichoptera = mean(trichoptera),
            mean_isopoda = mean(isopoda),
            mean_neuroptera = mean(neuroptera),
            mean_diplopoda = mean(diplopoda),
            mean_odonata = mean(odonata),
            mean_psocoptera = mean(psocoptera),
            mean_isoptera = mean(isoptera),
            mean_copeopoda = mean(copeopoda),
            mean_cladocera = mean(cladocera),
            mean_scorpions = mean(scorpions),
            mean_cyclopoida = mean(cyclopoida),
            mean_harpaticoida = mean(harpaticoida),
            mean_microptera = mean(microptera),
            n_genus = n())

#repeat but by predation type
carnivore_number_indivs_summary_pred <- carnivore_number_indivs %>% group_by(predation_method) %>%
  summarise(mean_diptera =mean(diptera.sum),
            mean_acarina = mean(acarina),
            mean_hymenoptera.not.formicidae = mean(hymenoptera.not.formicidae),
            mean_thysanoptera = mean(thysanoptera),
            mean_homoptera.sum = mean(homoptera.sum),
            mean_coleoptera = mean(coleoptera),
            mean_araneae = mean(araneae),
            mean_lepidoptera = mean(lepidoptera),
            mean_hemiptera = mean(hemiptera),
            mean_plecoptera = mean(plecoptera),
            mean_formicidae = mean(formicidae),
            mean_orthoptera = mean(orthoptera),
            mean_protura = mean(protura),
            mean_chilopoda = mean(chilopoda),
            mean_trichoptera = mean(trichoptera),
            mean_isopoda = mean(isopoda),
            mean_neuroptera = mean(neuroptera),
            mean_diplopoda = mean(diplopoda),
            mean_odonata = mean(odonata),
            mean_psocoptera = mean(psocoptera),
            mean_isoptera = mean(isoptera),
            mean_copeopoda = mean(copeopoda),
            mean_cladocera = mean(cladocera),
            mean_scorpions = mean(scorpions),
            mean_cyclopoida = mean(cyclopoida),
            mean_harpaticoida = mean(harpaticoida),
            mean_microptera = mean(microptera),
            n_predation_method = n())





