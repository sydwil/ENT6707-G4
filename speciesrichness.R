##Syd's attempts at order/group richness because we don't have species
##also! I softwrap long lines in the "Code" options, sorry if that's a struggle to read

##used Load_data.R

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
##this method won't work, I guess we have more of one species than another, BUT if I do it for each individual trap type, that's 3 graphs we can either put next to each other or easily compare!

sticky <- subset(testing, predation_method == "sticky_traps")
stickytab <- table(sticky$arthropod_group, sticky$predation_method)
stickysum<-colSums(stickytab)
print(stickysum)
##something is funky with this...

library(plotrix)
##https://www.flutterbys.com.au/stats/tut/tut13.2.html
#apply(sticky[,-1]>0,1,sum) this is a mess, so I'm going to remove some columns
sticky2 = subset(sticky, select = -c(genus , species, units, study))
apply(sticky2[,-1]>0,1,sum)
##this method isn't going to work with how the columns are laid out unfortunately
##so let's try again with a different method
library(plyr)
ddply (sticky,~ sites ,function(arthropod_group) {
  +   data.frame(RICHNESS=sum(arthropod_group[-1]>0))})
##getting closer, but I fear I am still not picking the correct variables.
#the good thing is I don't think this code is too awful, again the way our data frame is set up makes everything way more complicated