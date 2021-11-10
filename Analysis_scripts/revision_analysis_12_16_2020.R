if (.Platform$OS.type == "windows") {
  source("Z:\\Longitudinal_RAN_Reading_Meta_Analysis\\Meta_analysis_new_and_improved\\R_Scripts\\Analysis_scripts\\Effect_size_reshaping_1_27_2020.R")
} else {
  source("/Volumes/NortonLab/Longitudinal_RAN_Reading_Meta_Analysis/Meta_analysis_new_and_improved/R_Scripts/Analysis_scripts/Effect_size_reshaping_1_27_2020.R")
}

############################## Analysis #################################
library(metafor)
library(robumeta)
#library(clubSandwich)




dat2 <- escalc(measure="ZCOR", ri=cor, ni=total_n, data=effect_sizes_long_full, slab=umbrella) 
dat2$n_char <- as.character(dat2$total_n)

dat2$sda <- sqrt(dat2$vi)

# dat3 <- dat2 %>%
#   filter(!record_id %in% c("Smith Data (Controls) [1]", "Smith Data (Dyslexia Group) [1]"))

all_intercept <- robu(formula = final_e_size ~ 1, data = dat2, modelweights = "CORR",
                      studynum = umbrella, var.eff.size = vi)

print(all_intercept)
fisherz2r(all_intercept[["b.r"]])

m_colors <- robu(formula = final_e_size ~ 1, data = (dat2 %>% filter(ran_type == "Colors")), modelweights = "CORR",
                 studynum = umbrella, var.eff.size = vi)

m_objects <- robu(formula = final_e_size ~ 1, data = (dat2 %>% filter(ran_type == "Objects")), modelweights = "CORR",
                  studynum = umbrella, var.eff.size = vi)
  
m_letters <- robu(formula = final_e_size ~ 1, data = (dat2 %>% filter(ran_type == "Letters")), modelweights = "CORR",
                  studynum = umbrella, var.eff.size = vi)

m_numbers <- robu(formula = final_e_size ~ 1, data = (dat2 %>% filter(ran_type == "Numbers")), modelweights = "CORR",
                  studynum = umbrella, var.eff.size = vi)

m_comprehension <- robu(formula = final_e_size ~ 1, data = (dat2 %>% filter(comprehension == "yes_comprehension")), modelweights = "CORR",
                  studynum = umbrella, var.eff.size = vi)

m_timed <- robu(formula = final_e_size ~ 1, data = (dat2 %>% filter(timed == "timed")), modelweights = "CORR",
                  studynum = umbrella, var.eff.size = vi)

m_untimed <- robu(formula = final_e_size ~ 1, data = (dat2 %>% filter(timed == "not timed")), modelweights = "CORR",
                  studynum = umbrella, var.eff.size = vi)

m_fluency <- robu(formula = final_e_size ~ 1, data = (dat2 %>% filter(fluency == "fluency")), modelweights = "CORR",
                  studynum = umbrella, var.eff.size = vi)

m_single_word <- robu(formula = final_e_size ~ 1, data = (dat2 %>% filter(!is.na(nonword_single))), modelweights = "CORR",
                      studynum = umbrella, var.eff.size = vi)

m_nonword <- robu(formula = final_e_size ~ 1, data = (dat2 %>% filter(nonword_single == "Nonword")), modelweights = "CORR",
                  studynum = umbrella, var.eff.size = vi)

m_real_word <- robu(formula = final_e_size ~ 1, data = (dat2 %>% filter(nonword_single == "Word")), modelweights = "CORR",
                  studynum = umbrella, var.eff.size = vi)

m_efficiency <- robu(formula = final_e_size ~ 1, data = (dat2 %>% filter(efficiency == "efficiency")), modelweights = "CORR",
                     studynum = umbrella, var.eff.size = vi)

m_accuracy <- robu(formula = final_e_size ~ 1, data = (dat2 %>% filter(efficiency == "accuracy")), modelweights = "CORR",
                     studynum = umbrella, var.eff.size = vi)

m_diss <-  robu(formula = final_e_size ~ dissertation, data = dat2, modelweights = "CORR",
                studynum = umbrella, var.eff.size = vi)

n_item <- robu(formula = final_e_size ~ 1 + total_ran_items, data = dat2 %>% filter(!record_id %in% c('Cirino, 2018; Longitudinal Predict [1]', 'Lachance, 2006; A longitudinal analy [1]')),
           studynum = umbrella, var.eff.size = vi)

unique_tokens <- robu(formula = final_e_size ~ 1 + ran_item_unique, data = dat2 %>% filter(!record_id == '[S] Hood, 2005; the role of temporal [1]'),
                      studynum = umbrella, var.eff.size = vi)



for_table <-      list(all_intercept, m_colors, m_objects, m_letters, m_numbers, m_comprehension, m_fluency, m_timed, m_untimed, m_real_word, m_nonword, m_single_word, m_efficiency, m_accuracy)
for_table_m_names <- c("all_intercept", "m_colors", "m_objects", "m_letters", "m_numbers", 'm_comprehension', "m_fluency", "m_timed", "m_untimed", 'm_real_word', 'm_nonword', 'm_single_word', 'm_efficiency', 'm_accuracy')



results_table <- function(l, names){
  z <- data.frame()
  for(i in 1:length(l)){
    z1 <- data.frame(
      model = i,
      n = l[[i]][["N"]],
      k = l[[i]][["M"]],
      i2 = l[[i]][["mod_info"]][["I.2"]],
      t2 = l[[i]][["mod_info"]][["tau.sq"]],
      reg = l[[i]][["reg_table"]],
      type = "model")
    z <- rbind(z, z1)  
  }

  #round all numeric columns
  nums <- vapply(z, is.numeric, FUN.VALUE = logical(1))
  z[,nums] <- round(z[,nums], digits = 2)
  z$CI <- paste0("[", z$reg.CI.L, " ", z$reg.CI.U, "]")
  z <- z %>%
    select(-type, -reg.CI.L, -reg.CI.U, -reg.sig)
  z$model <- names
  return(z)
}

output <- results_table(for_table, for_table_m_names)
output$reg.b.r <- round(fisherz2r(output$reg.b.r), 2)
output

















#outlier checking
study_means <- dat2 %>% 
  group_by(umbrella) %>%
  summarise(e_size_outlier = mean(final_e_size),
            total_ran_items_outlier = mean(total_ran_items),
            ran_item_unique_outlier = mean(ran_item_unique),
            final_timepoint_outlier = mean(final_timepoint),
            initial_timepoint_outlier = mean(initial_timepoint),
            timelapse_outlier = mean(timelapse),
            total_n_outlier = mean(total_n))

boxplot(study_means$e_size_outlier, na.rm = T)
boxplot(study_means$total_ran_items_outlier, na.rm = T)
boxplot(study_means$ran_item_unique_outlier, na.rm = T)
boxplot(study_means$timelapse_outlier, na.rm = T)
boxplot(study_means$initial_timepoint_outlier, na.rm = T)
boxplot(study_means$final_timepoint_outlier, na.rm = T)
boxplot(study_means$total_n_outlier, na.rm = T)

outlier_id <- function(x){
  lower <- quantile(x, .025, na.rm = T)
  upper <- quantile(x, .975, na.rm = T)
  outlier_ind <- which(x < lower | x > upper)
  return(outlier_ind)
}

study_means[c(outlier_id(study_means$e_size_outlier)),]
study_means[c(outlier_id(study_means$total_ran_items_outlier)),]
study_means[c(outlier_id(study_means$ran_item_unique_outlier)),]
study_means[c(outlier_id(study_means$timelapse_outlier)),]
study_means[c(outlier_id(study_means$initial_timepoint_outlier)),]
study_means[c(outlier_id(study_means$final_timepoint_outlier)),]
study_means[c(outlier_id(study_means$total_n_outlier)),]


