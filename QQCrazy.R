## data alterations for qq plot of proportion means
Alter_CarnPropMean <- as.data.frame(t(carnivore_proportion_means))
Alter_CarnPropMean <- Alter_CarnPropMean %>% rename_at(1,~"Means")
qq_proportionmeans <- qqnorm(Alter_CarnPropMean$Means)

## data alterations for qq plot of count means
Alter_CarnCntMean <- as.data.frame(t(carnivore_count_means))
Alter_CarnCntMean <- Alter_CarnCntMean %>% rename_at(1, ~"Means")
qq_countmeans <- qqnorm(Alter_CarnCntMean$Means)

### I want to do invidiual qq plots for the specific counts/proportions grouped by predator methods, but IDK if it's worth it
### or usefull 
altCrnCtGrpNoSE<- carnivore_count_grouped %>% select(-(SE_diptera:n_predation_method))
CrnCtGrpNoSE<- as.data.frame(t(altCrnCtGrpNoSE))
CrnCtGrpNoSE<- CrnCtGrpNoSE %>% `colnames<-`(c("Active_trapping", "Pitchers", "Sticky_Traps"))
CrnCtGrpNoSE<- CrnCtGrpNoSE %>% filter(!row_number() %in% c(1))
CrnCtGrpNoSE<- CrnCtGrpNoSE %>% mutate(Active_trapping = gsub(" ","", Active_trapping))
CrnCtGrpNoSE<- CrnCtGrpNoSE %>% mutate(Pitchers = gsub(" ","", Pitchers))
CrnCtGrpNoSE<- CrnCtGrpNoSE %>% mutate(Sticky_Traps = gsub(" ","", Sticky_Traps))

hist(altCrnCtGrpNoSE$active_trapping)

qq_CtActive <- qqnorm(CrnCtGrpNoSE$Active_trapping)
qq_CtPitcher <- qqnorm(CrnCtGrpNoSE$Pitchers)
qq_CtSticky <- qqnorm(CrnCtGrpNoSE$Sticky_Traps)



CrnPrpGrpNoSE<- carnivore_proportion_grouped %>% select(-(SE_diptera:n_predation_method))
CrnPrpGrpNoSE<- as.data.frame(t(CrnPrpGrpNoSE))
CrnPrpGrpNoSE<- CrnPrpGrpNoSE %>% `colnames<-`(c("Active_trapping", "Pitchers", "Sticky_Traps"))
CrnPrpGrpNoSE<- CrnPrpGrpNoSE %>% filter(!row_number() %in% c(1))
CrnPrpGrpNoSE<- CrnPrpGrpNoSE %>% mutate(Active_trapping = gsub(" ","", Active_trapping))
CrnPrpGrpNoSE<- CrnPrpGrpNoSE %>% mutate(Pitchers = gsub(" ","", Pitchers))
CrnPrpGrpNoSE<- CrnPrpGrpNoSE %>% mutate(Sticky_Traps = gsub(" ","", Sticky_Traps))

qq_PrpActive <- qqnorm(CrnPrpGrpNoSE$Active_trapping)
qq_PrpPitcher <- qqnorm(CrnPrpGrpNoSE$Pitchers)
qq_PrpSticky <- qqnorm(CrnPrpGrpNoSE$Sticky_Traps)


# found this after a quick google, but this would be like fitting a qqnorm plot
# but for comparing against a Poisson distribution
library('fitdistrplus')
plot(fitdist(carnivore_number_indivs$diptera.sum,"pois"))
plot(fitdist(carnivore_number_indivs$coleoptera,"pois"))
