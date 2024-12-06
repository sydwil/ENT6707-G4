
#Just follow the "Load_data" code to get everything needed for running this
library(lme4)
library(car)
library(emmeans)
library(plotrix)
library(ggpubr)

#Let's look at the data first
carnie_count <-  carnivore %>% filter(units == "number_of_individuals")

#We need to get the data such that every count number is in one long column, and
#what arthropod group and what study it is are each in a separate column as 
#identifiers... gotta go from "wide" data to "long" data
str(carnie_count)
carnie_count[,5] # just trying to figure out what is our first count data's
                 # column number (diptera.sum) (it's 5)

carnie_count[,c(5,6,7,8,10,11,12,13,14,16)] # this gives us everything from diptera to lepidoptera

carnie_count[,16]
carnie_count$formicidae

testing <- carnie_count %>%
  pivot_longer(
    cols = c(5,6,7,8,10,11,12,13,14,16),
    names_to = "arthropod_group",
    values_to = "count",
    cols_vary = "slowest",) %>%
  as.data.frame()


# I'm going to plot each of the major arthropod groups and color by predation
ggplot(testing)+
  geom_jitter(aes(x = predation_method, y = count,  col = arthropod_group))+
  scale_color_discrete(name = "", labels = c("Acarina", "Araneae",
                                           "Coleoptera", "Collembola",
                                           "Diptera", "Homoptera",
                                           "Hemiptera", "Hymenoptera",
                                           "Lepidoptera"))+
  xlab("Predation method") +
  ylab("Catch count") +
  theme_bw()

#redo with no zeros


# I want to see what the data looks like without those two outliers way up high
testing_no_outliers <- filter(testing, count < 2000)

ggplot(testing_no_outliers)+
  geom_jitter(aes(x = predation_method, y = count,  col = arthropod_group))+
  theme_bw()






### Figure 1 ##################################################################
testing <- carnie_count %>%
  pivot_longer(
    cols = c(5,6,7,8,10,11,12,13,14,16),
    names_to = "arthropod_group",
    values_to = "count",
    cols_vary = "slowest",) %>%
  as.data.frame()

# let's do it with non-zeros
testing_no_zeros <- filter(testing, count != 0)

ggplot(testing_no_zeros)+
  geom_jitter(aes(x = predation_method, y = count, col = arthropod_group,
                  shape = arthropod_group))+
  xlab("Predation method") +
  ylab("Catch count") +
  scale_shape_manual(values = c(15, 16, 17, 18, 15, 16, 17, 18, 15, 16),
                     name = "", labels = c("Acarina", "Araneae",
                                           "Coleoptera", "Collembola",
                                           "Diptera", "Formicidae", "Homoptera",
                                           "Hemiptera", "Hymenoptera*",
                                           "Lepidoptera"))+
  # scale_shape_discrete(name = "", labels = c("Acarina", "Araneae",
  #                      "Coleoptera", "Collembola",
  #                      "Diptera", "Formicidae", "Homoptera",
  #                      "Hemiptera", "Hymenoptera*",
  #                      "Lepidoptera"))+
  scale_color_discrete(name = "", labels = c("Acarina", "Araneae",
                                           "Coleoptera", "Collembola",
                                           "Diptera", "Formicidae", "Homoptera",
                                           "Hemiptera", "Hymenoptera*",
                                           "Lepidoptera"))+
  scale_x_discrete(labels = c("Active trapping", "Pitchers", "Sticky traps"))+
theme_bw()


#### Figure 1 Remake ###########################################################
figure_1A <- ggplot(testing_no_zeros)+
  geom_jitter(aes(x = predation_method, y = count, fill = arthropod_group,
                  shape = arthropod_group, size = arthropod_group))+
  xlab("Predation method") +
  ylab("Catch count") +
  scale_shape_manual(values = c(21, 22, 23, 24, 21, 22, 23, 24, 21, 22),
                     name = "", labels = c("Acarina", "Araneae",
                                           "Coleoptera", "Collembola",
                                           "Diptera", "Formicidae", "Homoptera",
                                           "Hemiptera", "Hymenoptera*",
                                           "Lepidoptera"))+
  scale_fill_manual(name = "", labels = c("Acarina", "Araneae",
                                             "Coleoptera", "Collembola",
                                             "Diptera", "Formicidae", "Homoptera",
                                             "Hemiptera", "Hymenoptera*",
                                             "Lepidoptera"),
                    values = c("black", "#440154FF", "#453781FF", "#33638DFF","#238A8DFF",
                               "#20A387FF", "#3CBB7FFF", "#73D055FF", "#B8DE29FF", "#DCE319FF"))+
  scale_size_manual(name = "", labels = c("Acarina", "Araneae",
                                         "Coleoptera", "Collembola",
                                         "Diptera", "Formicidae", "Homoptera",
                                         "Hemiptera", "Hymenoptera*",
                                         "Lepidoptera"),
                   values = c(2, 1.5, 1.5, 1.5, 2, 1.5, 1.5, 1.5, 2, 1.5))+
  scale_x_discrete(labels = c("Active trapping", "Pitchers", "Sticky traps"))+
  theme_bw()

#"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""



#run this without the renamed legend just to make sure all the colors still 
# match
ggplot(testing_no_zeros)+
  geom_jitter(aes(x = predation_method, y = count,  col = arthropod_group))+
  xlab("Predation method") +
  ylab("Catch count") +
  theme_bw()



# Make bar graph of means
# remove zeros
  # this is trickier than I initially thought, because I just want to calculate
  # means without including zeros. However, if I get rid of rows by saing, for 
  # example: filter(carnie_count, diptera.sum != 0), than I will also get rid
  # of any values in that same row for other arthropods that are nonzero
  # see "Practice code" at bottom of this script for workaround


### Figure 2 ###################################################################

#replace zeros with NA values
carnie_count[carnie_count == 0] <- NA


carnie_count_less_means <- carnie_count %>% group_by(predation_method) %>%
  summarise(mean_diptera = round(mean(diptera.sum, na.rm = TRUE),2),
            mean_acarina = round(mean(acarina, na.rm = TRUE),2),
            mean_hymenoptera.not.formicidae = round(mean(hymenoptera.not.formicidae, na.rm = TRUE),2),
            #mean_thysanoptera = round(mean(thysanoptera),2),
            mean_homoptera = round(mean(homoptera.sum, na.rm = TRUE),2),
            mean_coleoptera = round(mean(coleoptera, na.rm = TRUE),2),
            mean_collembola = round(mean(collembola, na.rm = TRUE),2),
            mean_araneae = round(mean(araneae, na.rm = TRUE),2),
            mean_lepidoptera = round(mean(lepidoptera, na.rm = TRUE),2),
            mean_hemiptera = round(mean(hemiptera, na.rm = TRUE),2),
            #mean_plecoptera = round(mean(plecoptera),2),
            mean_formicidae = round(mean(formicidae, na.rm = TRUE),2))
#mean_orthoptera = round(mean(orthoptera),2))


carnie_count_less_SE <- carnie_count %>% group_by(predation_method) %>%
  summarise(SE_diptera = round(std.error(diptera.sum, na.rm = TRUE),2),
            SE_acarina = round(std.error(acarina, na.rm = TRUE),2),
            SE_hymenoptera.not.formicidae = round(std.error(hymenoptera.not.formicidae, na.rm = TRUE),2),
            #SE_thysanoptera = round(std.error(thysanoptera),2),
            SE_homoptera = round(std.error(homoptera.sum, na.rm = TRUE),2),
            SE_coleoptera = round(std.error(coleoptera, na.rm = TRUE),2),
            SE_collembola = round(std.error(collembola, na.rm = TRUE),2),
            SE_araneae = round(std.error(araneae, na.rm = TRUE),2),
            SE_lepidoptera = round(std.error(lepidoptera, na.rm = TRUE),2),
            SE_hemiptera = round(std.error(hemiptera, na.rm = TRUE),2),
            #SE_plecoptera = round(std.error(plecoptera),2),
            SE_formicidae = round(std.error(formicidae, na.rm = TRUE),2))
#SE_orthoptera = round(std.error(orthoptera),2))

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


Figure_1B <- ggplot(carnie_count_less_graph, aes(x = predation_method, y = mean, fill = arthropod_group)) +
  geom_bar(stat = "identity", position = "dodge", col = "black") + 
  geom_errorbar(aes(ymin = mean - SE, ymax = mean + SE),
                position = "dodge", color = "gray") +
  xlab("Predation method") +
  ylab("Mean catch") +
  scale_fill_manual(name = "", labels = c("Acarina", "Araneae",
                                             "Coleoptera", "Collembola",
                                             "Diptera", "Formicidae", "Homoptera",
                                             "Hemiptera", "Hymenoptera*",
                                             "Lepidoptera"),
                      values = c("black", "#440154FF", "#453781FF", "#33638DFF","#238A8DFF",
                                 "#20A387FF", "#3CBB7FFF", "#73D055FF", "#B8DE29FF", "#DCE319FF"))+
  scale_x_discrete(labels = c("Active trapping", "Pitchers", "Sticky traps"))+
  theme_bw()


### New Figure 1 (combined old 1 and 2) ########################################
ggarrange(figure_1A, Figure_1B, common.legend = TRUE, legend = "right",
          labels = c("A", "B"), ncol = 1, nrow = 2)

# used PDF preview of 5.88 x 4 inches to get image for document








### Old code, don't use ######################################################
#### Try to get a barplot #####################################################
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
  summarise(SE_diptera = round(std.error(diptera.sum, na.rm = TRUE),2),
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

### Practice code ##############################################################
set.seed(1234)
test <- data.frame(Group = rep(c("A", "B"), 5), 
                   Val = replicate(10, sample(c(0:3), 1, replace = TRUE)))

test %>% 
  group_by(Group) %>%
  filter(Val != 0)

  # this doesn't work becuase it removes rows (7 rows in output vs 10 in "test")

# try changing all zeros to NA
test[test == 0] <- NA
test %>% 
  group_by(Group) %>%
  summarise(Mean_Val = mean(Val, na.rm = TRUE))

#check it

test
# compare these two
mean(c(3, 1, 3, NA, 1), na.rm = TRUE)
mean(c(3, 1, 3, 1))
  




colMeans(replace(df, !df, NA), na.rm = TRUE)

#example code from: https://stackoverflow.com/questions/62125500/r-get-the-means-for-all-columns-in-a-data-frame-while-ignoring-zeroes
library(dplyr)
df %>%
  summarise(Mean = mean(Val, na_if(., 0), na.rm = TRUE)))






%>%
  summarise(Mean = mean(Val))

test %>% 
  group_by(Group) %>%
  filter(Val != 0)#To get the mean without zeros for each group and for the sign - we can create
# a new variable for sign and now group_by group and sign:
  
  test %>% 
  mutate(sign = ifelse(Val > 0, "positive", "negative")) %>% 
  group_by(Group, sign) %>%
  filter(Val != 0) %>%
  summarise(Mean = mean(Val))




