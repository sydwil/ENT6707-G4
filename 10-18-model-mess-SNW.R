##I used the load data r file to get to this point, so all function names will be from that document
carnivore_not_percentage
library(ggplot2)
library(car)
##I'm just checking to see if I can set up a model with predation sum as a predictor of diptera total
testmod <- lm(diptera.sum~predation_method, data = carnivore_not_percentage)
anova(testmod)
summary(testmod)
testmod2 <- lm(diptera.sum ~ predation_method + species, data = carnivore_not_percentage)
summary(testmod2)
Anova(testmod2)
## I haven't done much today, got sidetracked but I'll add to this!!!