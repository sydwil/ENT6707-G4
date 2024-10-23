library(lme4)

############################################################################################################
##DATA PREPERATION CODE BLOCK###

### creates a new dataframe that has been converted to proportions based off summing up the total individuals 
### captured by genus in each study 
carnivore_count <- filter(carnivore, units == "number_of_individuals")

Carnivore_CountToPercent <- carnivore_count %>% rowwise(study)
Carnivore_CountToPercent <- Carnivore_CountToPercent %>% mutate(total = sum(c_across(diptera.sum:mollusca)))
Carnivore_CountToPercent %>% summarise(total = sum(c_across(diptera.sum:mollusca)))

Carnivore_CountToPercent <- Carnivore_CountToPercent %>% mutate(total = sum(c_across(diptera.sum:mollusca)))%>%
  ungroup()%>%
  mutate(across(diptera.sum:mollusca, ~ . / total))


###This creates a new column in the dataframe that shows the richness of prey capture 
###per genus per study 
###This should also work when applied to any other dataframe with the count data, like "carnivore", 
###but i haven't tested that yet 

Carnivore_CountToPercent  <- Carnivore_CountToPercent %>% 
  rowwise()%>%
  mutate(
  Richness = length(which(c_across(diptera.sum:mollusca) > 0)))

rlang::last_trace()

#BEWARE, all this loop does is pulverize your dataframe, i'm keeping it in case I ever need it again 

x <- 1

Carnivore_CountToPercent <- while (x>=5){
  Carnivore_CountToPercent[i, ] <- ((Carnivore_CountToPercent[i, ] / Carnivore_CountToPercent$rowsums) * 100)
  x <- x + 1
  if (x == 54){
    break
  }
}
####################################################################################################################







#########################################################################################################################
###DATA ANALYSIS CODE BLOCK
###Requires Carnivore_CountTOPercent to have been transformed to be proportions & to have richness info to work 
###Capture data 
ggplot(data=Carnivore_CountToPercent, aes(x=diptera.sum, color = predation_method))+
  geom_histogram(fill="white")

ggplot(data=Carnivore_CountToPercent, aes(x=formicidae, color = predation_method))+
  geom_histogram(fill="white")

ggplot(data=Carnivore_CountToPercent, aes(x=hymenoptera.not.formicidae, color = predation_method))+
  geom_histogram(fill="white")


testmod1 <- lm(diptera.sum~predation_method, data = Carnivore_CountToPercent)
anova(testmod1)
summary(testmod1)
plot(testmod1)


testmod2 <- lm(formicidae~predation_method, data = Carnivore_CountToPercent)
anova(testmod2)
summary(testmod2)
plot (testmod2)

testmod3 <- lm(hymenoptera.not.formicidae~predation_method, data = Carnivore_CountToPercent)
anova(testmod3)
summary(testmod3)
plot (testmod3)


### Richness data testing 
ggplot(data=Carnivore_CountToPercent, aes(x=Richness, color = predation_method))+
  geom_histogram(fill="white")

testmod1R <- lm(Richness~predation_method, data = Carnivore_CountToPercent)
anova(testmod1R)
summary(testmod1R)
plot(testmod1R)


ggplot(data=Carnivore_CountToPercent, aes(x=Richness, color = genus))+
  geom_histogram(fill="white")

testmod2R <- lm(Richness~genus, data = Carnivore_CountToPercent)
anova(testmod2R)
summary(testmod2R)
plot(testmod2R)




