##Syd's attempts at order/group richness because we don't have species
##used Load_data.R
##working off a tutorial from --> https://rpubs.com/mbh038/719881
carnie_count <-  carnivore %>% filter(units == "number_of_individuals")
head(carnie_count)
glimpse(carnie_count)
ps<-table(carnie_count$diptera.sum,carnie_count$predation_method)
psr<-colSums(ps)
##so we could technically do this for every order, but obviously it would be much nicer if we could do it faster?
library(tidyverse)