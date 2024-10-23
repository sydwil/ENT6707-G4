##Syd's attempts at order/group richness because we don't have species
##also! I softwrap long lines in the "Code" options, sorry if that's a struggle to read
##used Load_data.R to give myself a shortcut
carnivore <- read.csv("/Users/schoolpersona/Desktop/ENT6707/ENT6707_DataAnalysis/Group project folder/ENT6707-G4/G4_hf111-01-prey.csv", header = TRUE, 
                      na.strings = "NA")
##working off a tutorial from --> https://rpubs.com/mbh038/719881
##this data bit is loaded from Analysis.R
carnie_count <-  carnivore %>% filter(units == "number_of_individuals")

carnie_count[,5:13] # this gives us everything from diptera to lepidoptera

testing <- carnie_count %>%
  pivot_longer(
    cols = 5:13,
    names_to = "arthropod_group",
    values_to = "count",
    cols_vary = "slowest",) %>%
  as.data.frame()
glimpse(testing)
##I miss my big PC screen so much.
ps<-table(testing$arthropod_group,carnie_count$predation_method)
##this method won't work, I guess we have more of one species than another, BUT if I do it for each individual 
psr<-colSums(ps)
##so we could technically do this for every order, but obviously it would be much nicer if we could do it faster?
library(tidyverse)
install.packages("lme4")
yes
install.packages("gfortran")
