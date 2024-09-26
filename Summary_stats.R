# These are for initial graphs and subset summary statistics. Please first run
# the code found on "Load_data.R" to be able to run this code.

# Let's start out by just comparing the arthropod options where most studies have
# nonzero data: diptera, acarina, collembola, hymenoptera.not.formicidae,
# thysanoptera, homoptera, coleoptera, araneae, lepidoptera, hemiptera, 
# formicidae, orthoptera
  # - Tom

library(plotrix)

### Proportion studies #########################################################
carnivore_proportion_means <- carnivore_percentage %>%
  summarise(mean_diptera = round(mean(diptera.sum),2),
            mean_acarina = round(mean(acarina),2),
            mean_hymenoptera.not.formicidae = round(mean(hymenoptera.not.formicidae),2),
            mean_thysanoptera = round(mean(thysanoptera),2),
            mean_homoptera.sum = round(mean(homoptera.sum),2),
            mean_coleoptera = round(mean(coleoptera),2),
            mean_araneae = round(mean(araneae),2),
            mean_lepidoptera = round(mean(lepidoptera),2),
            mean_hemiptera = round(mean(hemiptera),2),
            mean_plecoptera = round(mean(plecoptera),2),
            mean_formicidae = round(mean(formicidae),2),
            mean_orthoptera = round(mean(orthoptera),2))

carnivore_proportion_SE <- carnivore_percentage %>%
  summarise(SE_diptera = round(std.error(diptera.sum),2),
            SE_acarina = round(std.error(acarina),2),
            SE_hymenoptera.not.formicidae = round(std.error(hymenoptera.not.formicidae),2),
            SE_thysanoptera = round(std.error(thysanoptera),2),
            SE_homoptera.sum = round(std.error(homoptera.sum),2),
            SE_coleoptera = round(std.error(coleoptera),2),
            SE_araneae = round(std.error(araneae),2),
            SE_lepidoptera = round(std.error(lepidoptera),2),
            SE_hemiptera = round(std.error(hemiptera),2),
            SE_plecoptera = round(std.error(plecoptera),2),
            SE_formicidae = round(std.error(formicidae),2),
            SE_orthoptera = round(std.error(orthoptera),2))

# pivot_longer() function helps condenses the tables above into a summarized
# one below. From "wide" data to "long" data (usually we wanna go the other way)
test1 <- carnivore_proportion_means %>%
  pivot_longer(
    cols = starts_with("mean"),
    names_to = "arthropod_group",
    names_prefix = "mean_",
    values_to = "mean") %>%
  as.data.frame()

test2 <- carnivore_proportion_SE %>%
  pivot_longer(
    cols = starts_with("SE"),
    names_to = "arthropod_group",
    names_prefix = "SE_",
    values_to = "SE") %>% 
  as.data.frame()

# combine the two!
summary_stats_proportion <- test1
summary_stats_proportion$SE <- test2$SE


#### Group by predation type (for analyses) ####################################
carnivore_proportion_grouped <- carnivore_percentage %>% group_by(predation_method) %>%
summarise(mean_diptera = round(mean(diptera.sum),2),
          mean_acarina = round(mean(acarina),2),
          mean_hymenoptera.not.formicidae = round(mean(hymenoptera.not.formicidae),2),
          mean_thysanoptera = round(mean(thysanoptera),2),
          mean_homoptera.sum = round(mean(homoptera.sum),2),
          mean_coleoptera = round(mean(coleoptera),2),
          mean_araneae = round(mean(araneae),2),
          mean_lepidoptera = round(mean(lepidoptera),2),
          mean_hemiptera = round(mean(hemiptera),2),
          mean_plecoptera = round(mean(plecoptera),2),
          mean_formicidae = round(mean(formicidae),2),
          mean_orthoptera = round(mean(orthoptera),2),
          
          SE_diptera = round(std.error(diptera.sum),2),
          SE_acarina = round(std.error(acarina),2),
          SE_hymenoptera.not.formicidae = round(std.error(hymenoptera.not.formicidae),2),
          SE_thysanoptera = round(std.error(thysanoptera),2),
          SE_homoptera.sum = round(std.error(homoptera.sum),2),
          SE_coleoptera = round(std.error(coleoptera),2),
          SE_araneae = round(std.error(araneae),2),
          SE_lepidoptera = round(std.error(lepidoptera),2),
          SE_hemiptera = round(std.error(hemiptera),2),
          SE_plecoptera = round(std.error(plecoptera),2),
          SE_formicidae = round(std.error(formicidae),2),
          SE_orthoptera = round(std.error(orthoptera),2),
          n_predation_method = n())




### Total count studies ########################################################
carnivore_count_means <- carnivore_number_indivs %>% 
  summarise(mean_diptera = round(mean(diptera.sum),2),
            mean_acarina = round(mean(acarina),2),
            mean_hymenoptera.not.formicidae = round(mean(hymenoptera.not.formicidae),2),
            mean_thysanoptera = round(mean(thysanoptera),2),
            mean_homoptera.sum = round(mean(homoptera.sum),2),
            mean_coleoptera = round(mean(coleoptera),2),
            mean_araneae = round(mean(araneae),2),
            mean_lepidoptera = round(mean(lepidoptera),2),
            mean_hemiptera = round(mean(hemiptera),2),
            mean_plecoptera = round(mean(plecoptera),2),
            mean_formicidae = round(mean(formicidae),2),
            mean_orthoptera = round(mean(orthoptera),2))

carnivore_count_SE <- carnivore_number_indivs %>%
  summarise(SE_diptera = round(std.error(diptera.sum),2),
            SE_acarina = round(std.error(acarina),2),
            SE_hymenoptera.not.formicidae = round(std.error(hymenoptera.not.formicidae),2),
            SE_thysanoptera = round(std.error(thysanoptera),2),
            SE_homoptera.sum = round(std.error(homoptera.sum),2),
            SE_coleoptera = round(std.error(coleoptera),2),
            SE_araneae = round(std.error(araneae),2),
            SE_lepidoptera = round(std.error(lepidoptera),2),
            SE_hemiptera = round(std.error(hemiptera),2),
            SE_plecoptera = round(std.error(plecoptera),2),
            SE_formicidae = round(std.error(formicidae),2),
            SE_orthoptera = round(std.error(orthoptera),2))

# pivot_longer() function helps condenses the tables above into a summarized
# one below. From "wide" data to "long" data (usually we wanna go the other way)
test3 <- carnivore_count_means %>%
  pivot_longer(
    cols = starts_with("mean"),
    names_to = "arthropod_group",
    names_prefix = "mean_",
    values_to = "mean") %>%
  as.data.frame()

test4 <- carnivore_count_SE %>%
  pivot_longer(
    cols = starts_with("SE"),
    names_to = "arthropod_group",
    names_prefix = "SE_",
    values_to = "SE") %>% 
  as.data.frame()

# combine the two!
summary_stats_count <- test3
summary_stats_count$SE <- test4$SE


#### Group by predation type (for analyses) ####################################
carnivore_count_grouped <- carnivore_number_indivs %>% group_by(predation_method) %>%
  summarise(mean_diptera = round(mean(diptera.sum),2),
            mean_acarina = round(mean(acarina),2),
            mean_hymenoptera.not.formicidae = round(mean(hymenoptera.not.formicidae),2),
            mean_thysanoptera = round(mean(thysanoptera),2),
            mean_homoptera.sum = round(mean(homoptera.sum),2),
            mean_coleoptera = round(mean(coleoptera),2),
            mean_araneae = round(mean(araneae),2),
            mean_lepidoptera = round(mean(lepidoptera),2),
            mean_hemiptera = round(mean(hemiptera),2),
            mean_plecoptera = round(mean(plecoptera),2),
            mean_formicidae = round(mean(formicidae),2),
            mean_orthoptera = round(mean(orthoptera),2),
            
            SE_diptera = round(std.error(diptera.sum),2),
            SE_acarina = round(std.error(acarina),2),
            SE_hymenoptera.not.formicidae = round(std.error(hymenoptera.not.formicidae),2),
            SE_thysanoptera = round(std.error(thysanoptera),2),
            SE_homoptera.sum = round(std.error(homoptera.sum),2),
            SE_coleoptera = round(std.error(coleoptera),2),
            SE_araneae = round(std.error(araneae),2),
            SE_lepidoptera = round(std.error(lepidoptera),2),
            SE_hemiptera = round(std.error(hemiptera),2),
            SE_plecoptera = round(std.error(plecoptera),2),
            SE_formicidae = round(std.error(formicidae),2),
            SE_orthoptera = round(std.error(orthoptera),2),
            n_predation_method = n())

### Write to Excel #############################################################
#! NOTE !
# This code is written to create an Excel file in this RStudio project,
# We should only run this if we change the code ahead of this, because otherwise
# I'll have the resulting Excel file pushed to the GitHub
  
library(writexl)

summary_stats_count$study_measurements <- "counts"
summary_stats_count$study_observations <- nrow(carnivore_number_indivs)

summary_stats_proportion$study_measurements <- "proportions"
summary_stats_proportion$study_observations <- nrow(carnivore_percentage)

summary_stats <- rbind(summary_stats_count, summary_stats_proportion)

write_xlsx(summary_stats, "G4_summary_stats_fixed.xlsx")
