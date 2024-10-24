#load librarys 
library(tidyr)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(stringr)
#load in dataset 
carnivore <- read.csv(file = "C:/Users/morga/Downloads/G4_hf111-01-prey.csv", header = TRUE, 
                      na.strings = "NA")
#make new genus category and fix typos 
carnivore <- carnivore %>% mutate(species = str_replace(species, " ", "_"))
carnivore$genus <- gsub("_.*","",  x = carnivore$species)
carnivore$genus
carnivore <- carnivore %>% 
  mutate(genus = replace(genus, genus == "Nepenthese", "Nepenthes"))
carnivore <- carnivore %>% 
  mutate(genus = replace(genus, genus == "Neentes", "Nepenthes"))
carnivore <- carnivore %>% 
  mutate(genus = replace(genus, genus == "Saracenia", "Sarracenia"))

#make new predation method column 
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

#filter out all those without number data 
carnivore_number <- carnivore %>% filter(units == "number_of_individuals")

library(car)
#Anova analysis
dipteramodel <- lm(diptera.sum~predation_method, data = carnivore_number)
anova(dipteramodel)
summary(dipteramodel)
library(emmeans)
dipmodelem <- emmeans(dipteramodel, ~predation_method)
pairs(dipmodelem)

#acarina as analysis 
acarinamodel <- lm(acarina~predation_method, data = carnivore_number)
anova(acarinamodel)
acmodelem <- emmeans(acarinamodel, ~predation_method)
pairs(acmodelem)

#collembola analysis "Springtails"
collembolamodel <- lm(collembola~predation_method, data = carnivore_number)
anova(collembolamodel)
comodelem <- emmeans(collembolamodel, ~predation_method)
pairs(comodelem)

#homoptera analysis " cicadas, aphids, leafhoppers, and other plant-feeding insects."
homomodel <- lm(homoptera.sum~predation_method, data = carnivore_number)
anova(homomodel)
homodelem <- emmeans(homomodel, ~predation_method)
pairs(homodelem)

### GLMER Models ###############################################################

#Just follow the "Load_data" code to get everything needed for running this
library(lme4)
library(car)

#Let's look at the data first
carnie_count <-  carnivore %>% filter(units == "number_of_individuals")

#We need to get the data such that every count number is in one long column, and
#what arthropod group and what study it is are each in a separate column as 
#identifiers... gotta go from "wide" data to "long" data
str(carnie_count)
carnie_count[,5] # just trying to figure out what is our first count data's
                 # column number (diptera.sum) (it's 5)

carnie_count[,5:13] # this gives us everything from diptera to lepidoptera

testing <- carnie_count %>%
  pivot_longer(
    cols = 5:13,
    names_to = "arthropod_group",
    values_to = "count",
    cols_vary = "slowest",) %>%
  as.data.frame()


# I'm going to plot each of the major arthropod groups and color by predation
ggplot(testing)+
  geom_jitter(aes(x = predation_method, y = count,  col = arthropod_group))+
  theme_bw()

# I want to see what the data looks like without those two outliers way up high
testing_no_outliers <- filter(testing, count < 2000)

ggplot(testing_no_outliers)+
  geom_jitter(aes(x = predation_method, y = count,  col = arthropod_group))+
  theme_bw()

# let's do it again with non-zeros
testing_no_zeros <- filter(testing, count != 0)

ggplot(testing_no_zeros)+
  geom_jitter(aes(x = predation_method, y = count,  col = arthropod_group))+
  theme_bw()


# fit generalized linear mixed effects model (allows for random intercepts and
# no-normal distribution) (we assume a Poisson distribution)
str(carnie_count)

#### Fit diptera ###############################################################
install.packages("Rcpp")
library(Rcpp)
dip_noZeros <- filter(carnie_count, diptera.sum != 0)
fit_diptera <- glmer(diptera.sum ~ predation_method + (1|genus), 
                     family = poisson(link="log"), 
                     data = dip_noZeros)
summary(fit_diptera)
Anova(fit_diptera, type="III")
emmeans(fit_diptera, pairwise ~ predation_method)

#### Fit coleoptera ############################################################
col_nozeros <- filter(carnie_count, coleoptera != 0)
fit_coleoptera <- glmer(coleoptera ~ predation_method + (1|genus), 
                     family = poisson(link="log"), 
                     data = col_nozeros)
summary(fit_coleoptera)
Anova(fit_coleoptera, type="III")
emmeans(fit_coleoptera, pairwise ~ predation_method)

ranef(fit_coleoptera)

#### Fit formicidae ############################################################
summary(carnie_count$formicidae)


fit_formicidae <- glmer(formicidae ~ predation_method + (1|genus), 
                        family = poisson(link="log"), 
                        data = carnie_count)

#weird error... wanna look at the genera involved and the data
carnie_count$formicidae
formicidae_noZeros <- filter(carnie_count, formicidae != 0)
unique(formicidae_noZeros$predation_method)
# Looks like maybe it's because there are no counts of formicidae in active trapping?
fit_formicidae_noZeros <- glmer(formicidae ~ predation_method + (1|genus), 
                        family = poisson(link="log"), 
                        data = formicidae_noZeros)
summary(fit_formicidae_noZeros)
Anova(fit_formicidae_noZeros, type="III")
emmeans(fit_formicidae_noZeros, pairwise ~ predation_method)


########formicidae#######SIGNIFICANT!

fit_formicidae <- glm(formicidae ~ predation_method, 
                        family = poisson(link="log"), 
                        data = carnie_count)

summary(fit_formicidae)
Anova(fit_formicidae, type="III")
emmeans(fit_formicidae, pairwise ~ predation_method)

#### Fit hemiptera ############################################################
hem_nozero <- filter(carnie_count, hemiptera != 0)
fit_hemiptera <- glmer(hemiptera ~ predation_method + (1|genus), 
                        family = poisson(link="log"), 
                        data = hem_nozero)
summary(fit_hemiptera)
Anova(fit_hemiptera, type="III")
emmeans(fit_hemiptera, pairwise ~ predation_method)

########acarina####### SIGNIFICANT!
acarina_nozero <- filter(carnie_count, acarina != 0)
fit_acarina <- glmer(acarina ~ predation_method + (1|genus), 
                       family = poisson(link="log"), 
                       data = acarina_nozero)
summary(fit_acarina)
Anova(fit_acarina, type="III")
emmeans(fit_acarina, pairwise ~ predation_method)

#####hymenoptera#########
hymen_nozero <- filter(carnie_count, hymenoptera.not.formicidae !=0)
fit_hymen <-glmer(hymenoptera.not.formicidae ~ predation_method + (1|genus),
                  family = poisson(link= "log"),
                  data = hymen_nozero)
summary(fit_hymen)
Anova(fit_hymen, type = "III")
emmeans(fit_hymen, pairwise ~ predation_method)

#######thysanoptera##### SIGNIFICANT!!!
thy_nozero <- filter(carnie_count, thysanoptera !=0)
fit_thy <-glmer(thysanoptera ~ predation_method + (1|genus),
                  family = poisson(link= "log"),
                  data = thy_nozero)
summary(fit_thy)
Anova(fit_thy, type = "III")
emmeans(fit_thy, pairwise ~ predation_method)

#########aranae#######
aran_nozero <- filter(carnie_count, araneae !=0)
fit_aran <-glmer(araneae ~ predation_method + (1|genus),
                family = poisson(link= "log"),
                data = aran_nozero)
summary(fit_aran)
Anova(fit_aran, type = "III")
emmeans(fit_aran, pairwise ~ predation_method)

######lepidoptera######SIGNIFICANT!!! 
lepi_nozero <- filter(carnie_count, lepidoptera !=0)
fit_lepi <-glmer(lepidoptera ~ predation_method + (1|genus),
                 family = poisson(link= "log"),
                 data = lepi_nozero)
summary(fit_lepi)
Anova(fit_lepi, type = "III")
emmeans(fit_lepi, pairwise ~ predation_method)

####plecoptera########## NO other data, only one count 

######orthoptera#########SIGNIFICANT!!!!!!
or_nozero <- filter(carnie_count, orthoptera !=0)
fit_or <-glmer(orthoptera ~ predation_method + (1|genus),
                 family = poisson(link= "log"),
                 data = or_nozero)
summary(fit_or)
Anova(fit_or, type = "III")
emmeans(fit_or, pairwise ~ predation_method)

###### some of the other ones are single reports or less than three reports 
######not sure if we want to analyze those really....!!!!!!


library(plotrix)
### Try to get a barplot #####################################################
carnie_count_means <- carnie_count %>% group_by(predation_method) %>%
  summarise(mean_diptera = round(mean(diptera.sum),2),
            mean_acarina = round(mean(acarina),2),
            mean_hymenoptera.not.formicidae = round(mean(hymenoptera.not.formicidae),2),
            mean_thysanoptera = round(mean(thysanoptera),2),
            mean_homoptera = round(mean(homoptera.sum),2),
            mean_coleoptera = round(mean(coleoptera),2),
            mean_araneae = round(mean(araneae),2),
            mean_lepidoptera = round(mean(lepidoptera),2),
            mean_hemiptera = round(mean(hemiptera),2),
            mean_plecoptera = round(mean(plecoptera),2),
            mean_formicidae = round(mean(formicidae),2),
            mean_orthoptera = round(mean(orthoptera),2))
            
            
carnie_count_SE <- carnie_count %>% group_by(predation_method) %>%
  summarise(SE_diptera = round(std.error(diptera.sum),2),
            SE_acarina = round(std.error(acarina),2),
            SE_hymenoptera.not.formicidae = round(std.error(hymenoptera.not.formicidae),2),
            SE_thysanoptera = round(std.error(thysanoptera),2),
            SE_homoptera = round(std.error(homoptera.sum),2),
            SE_coleoptera = round(std.error(coleoptera),2),
            SE_araneae = round(std.error(araneae),2),
            SE_lepidoptera = round(std.error(lepidoptera),2),
            SE_hemiptera = round(std.error(hemiptera),2),
            SE_plecoptera = round(std.error(plecoptera),2),
            SE_formicidae = round(std.error(formicidae),2),
            SE_orthoptera = round(std.error(orthoptera),2))




str(as.data.frame(carnie_count_SE))
str(as.data.frame(carnie_count_means))




testing1 <- carnie_count_means %>%
  pivot_longer(
    cols = starts_with("mean"),
    names_to = "arthropod_group",
    names_prefix = "mean_",
    values_to = "mean") %>%
  as.data.frame()

testing2 <- carnie_count_SE %>%
  pivot_longer(
    cols = starts_with("SE"),
    names_to = "arthropod_group",
    names_prefix = "SE_",
    values_to = "SE") %>% 
  as.data.frame()

carnie_count_graph <- testing1
carnie_count_graph$SE <- testing2$SE


ggplot(carnie_count_graph)+
  geom_bar(aes(x = predation_method, y = mean, fill = arthropod_group), 
           stat = "identity", position = "dodge")+
  geom_errorbar(aes(x = predation_method, ymin = mean - SE, ymax = mean + SE),
                width = 0.2, position = position_dodge(12))+
  theme_bw()



# I'm going to repeat this, but eliminate some where it really looks like we
# have hardly any observations, and maybe let's just stick to Insecta?

carnie_count_less_means <- carnie_count %>% group_by(predation_method) %>%
  summarise(mean_diptera = round(mean(diptera.sum),2),
            #mean_acarina = round(mean(acarina),2),
            mean_hymenoptera.not.formicidae = round(mean(hymenoptera.not.formicidae),2),
            #mean_thysanoptera = round(mean(thysanoptera),2),
            mean_homoptera = round(mean(homoptera.sum),2),
            mean_coleoptera = round(mean(coleoptera),2),
            #mean_araneae = round(mean(araneae),2),
            mean_lepidoptera = round(mean(lepidoptera),2),
            mean_hemiptera = round(mean(hemiptera),2),
            #mean_plecoptera = round(mean(plecoptera),2),
            mean_formicidae = round(mean(formicidae),2))
            #mean_orthoptera = round(mean(orthoptera),2))


carnie_count_less_SE <- carnie_count %>% group_by(predation_method) %>%
  summarise(SE_diptera = round(std.error(diptera.sum),2),
            #SE_acarina = round(std.error(acarina),2),
            SE_hymenoptera.not.formicidae = round(std.error(hymenoptera.not.formicidae),2),
            #SE_thysanoptera = round(std.error(thysanoptera),2),
            SE_homoptera = round(std.error(homoptera.sum),2),
            SE_coleoptera = round(std.error(coleoptera),2),
            #SE_araneae = round(std.error(araneae),2),
            SE_lepidoptera = round(std.error(lepidoptera),2),
            SE_hemiptera = round(std.error(hemiptera),2),
            #SE_plecoptera = round(std.error(plecoptera),2),
            SE_formicidae = round(std.error(formicidae),2))
            #SE_orthoptera = round(std.error(orthoptera),2))

str(as.data.frame(carnie_count_less_SE))
str(as.data.frame(carnie_count_less_means))


testing11 <- carnie_count_less_means %>%
  pivot_longer(
    cols = starts_with("mean"),
    names_to = "arthropod_group",
    names_prefix = "mean_",
    values_to = "mean") %>%
  as.data.frame()

testing21 <- carnie_count_less_SE %>%
  pivot_longer(
    cols = starts_with("SE"),
    names_to = "arthropod_group",
    names_prefix = "SE_",
    values_to = "SE") %>% 
  as.data.frame()

carnie_count_less_graph <- testing11
carnie_count_less_graph$SE <- testing21$SE


ggplot(carnie_count_less_graph, aes(x = predation_method, y = mean, fill = arthropod_group)) +
  geom_bar(stat = "identity", position = "dodge") + 
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE),
                position = "dodge") + theme_bw()


#broken 
# ggplot(carnie_count_less_graph, aes(x = predation_method, y = mean))+
#   geom_bar(aes(x = predation_method, y = mean, fill = arthropod_group), 
#            color = "black",
#            stat = "identity", position = position_dodge())+
#   geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE),
#                 position = position_dodge(0.5),
#                 width = 0.1)+
#   theme(legend.position = "none")
# 
# head(carnie_count_less_graph)


##############
############## average richness 

#adds richness colum to carnie_count
Richness <- carnie_count %>%
  rowwise()%>%
  mutate(
    Richness = length(which(c_across(diptera.sum:mollusca) > 0)))
Richness_means <- Richness %>% group_by(predation_method)%>%
  summarise(MeanRichness = round(mean(Richness),2))
Richness_SE <- Richness %>% group_by(predation_method)%>%
  summarise(SERichness = round(std.error(Richness),2))

str(as.data.frame(Richness_SE))
str(as.data.frame(Richness_means))

Richness_Graph <- Richness_means
Richness_Graph$SE <- Richness_SE$SERichness

ggplot(Richness_Graph)+
  geom_bar(aes(x = predation_method, y = MeanRichness, fill = predation_method), 
           stat = "identity", position = "dodge")+
  geom_errorbar(aes(x = predation_method, ymin = MeanRichness - SE, ymax = MeanRichness + SE),
                width = 0.2, position = position_dodge(12))+
  theme_bw()




