##I used the load data r file to get to this point, so all function names will be from that document
carnivore_number_indivs
library(ggplot2)
library(car)
##I'm just checking to see if I can set up a model with predation sum as a predictor of diptera total
testmod <- lm(diptera.sum~predation_method, data = carnivore_number_indivs)
Anova(testmod)
summary(testmod)
##Basically what it looks like is the diptera don't show any difference in predation method between trap types, but this model also accounts for very little variation

##I'm not feeling very confident in my ability to fit a complicated model, BUT!
# Compute percentages
pitchers <- subset(carnivore_number_indivs, predation_method == "pitchers")
#summary of the columns instead of manually adding them, but then the output isn't one we can use in a graph easily
pitcher_sum <- pitchers %>% bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Total"))