source("/Volumes/NortonLab/Longitudinal_RAN_Reading_Meta_Analysis/Meta_analysis_new_and_improved/R_Scripts/Analysis_scripts/Effect_size_reshaping_1_27_2020.R")

############################## Analysis #################################
library(metafor)
library(robumeta)
library(clubSandwich)

effect_sizes_long_full <- effect_sizes_long_full %>%
  mutate(flip_cor = flip * cor)


summary_stats <- effect_sizes_long_full %>%
  group_by(umbrella) %>%
  summarise(max_per_study = max(total_n), age = mean(age_months), age_sd = mean(age_months_sd), 
            time_between = mean(timelapse))
(n <- sum(summary_stats$max_per_study))

(desc <- describe(summary_stats[,2:ncol(summary_stats)], na.rm = T))


paste0("The sample (n = ", n, ") was drawn from ", max(desc$n), " independent samples across 68 papers. For studies that reported an initial age, the mean initial age was ", round(desc$mean[2], 2),  " months with a mean SD of ", round(desc$mean[3], 2), ". The mean interval between initial and final timepoints was ", round(desc$mean[4], 2), " months, which is consistent with our prioritization of the Grade 2 timepoint.")


dat2 <- escalc(measure="ZCOR", ri=cor, ni=total_n, data=effect_sizes_long_full, slab=umbrella) 
dat2$sda <- sqrt(dat2$vi)


all_intercept <- robu(formula = final_e_size ~ 1, data = dat2,
                      studynum = umbrella, var.eff.size = vi)

print(all_intercept)
fisherz2r(all_intercept[["b.r"]])



m0 <- robu(formula = final_e_size ~ 1 + published, data = dat2,
           studynum = umbrella, var.eff.size = vi)

m00 <- robu(formula = final_e_size ~ 1 + emailed, data = dat2,
            studynum = umbrella, var.eff.size = vi)

m1 <- robu(formula = final_e_size ~ 1 + latent_cor, data = dat2,
           studynum = umbrella, var.eff.size = vi)


m2 <- robu(formula = final_e_size ~ 1 + initial_timepoint + final_timepoint, data = dat2,
           studynum = umbrella, var.eff.size = vi)


m3 <- robu(formula = final_e_size ~ 1 + ran_colors_objects, data = dat2,
            studynum = umbrella, var.eff.size = vi)

m4 <- robu(formula = final_e_size ~ 1 + ran_alphanumeric, data = dat2,
           studynum = umbrella, var.eff.size = vi)


m5 <- robu(formula = final_e_size ~ 1 + comprehension, data = dat2,
           studynum = umbrella, var.eff.size = vi)

m6 <- robu(formula = final_e_size ~ 1 + timed, data = dat2,
           studynum = umbrella, var.eff.size = vi)

m7  <- robu(formula = final_e_size ~ 1 + fluency, data = dat2,
            studynum = umbrella, var.eff.size = vi)


m11 <- robu(formula = final_e_size ~ 1 + ran_score, data = dat2,
            studynum = umbrella, var.eff.size = vi)

m12 <- robu(formula = final_e_size ~ 1 + ran_std, data = dat2,
            studynum = umbrella, var.eff.size = vi)

m13 <- robu(formula = final_e_size ~ 1 + ran_item_unique, data = dat2,
            studynum = umbrella, var.eff.size = vi)

m14 <- robu(formula = final_e_size ~ 1 + total_ran_items, data = dat2,
            studynum = umbrella, var.eff.size = vi)

m15 <- robu(formula = final_e_size ~ 1 + risk, data = dat2,
            studynum = umbrella, var.eff.size = vi)


m16 <- robu(formula = final_e_size ~ 1 + efficiency, data = dat2,
            studynum = umbrella, var.eff.size = vi)


m17 <- robu(formula = final_e_size ~ 1 + connected, data = dat2,
            studynum = umbrella, var.eff.size = vi)





dev.off()
forest.robu(all_intercept, es.lab = "citation", study.lab = "umbrella",
            "r" = flip_cor, "RAN_Type" = ran_type, "Reading Measure" = reading_measure, 
            "Length" = timespan)


rme <- rma(yi = dat3$final_e_size, vi = dat3$vi)
funnel(rme)

dat3 <- dat2 %>%
  filter(!citation == "Biddle, 1996")

dat_r <- dat2[order(dat2$reading_measure),]

dat_r <- dat_r %>%
  select(reading_measure, final_e_size, umbrella, ran_type)




#--------------------------------------------------
# Egg Sandwich (Robumeta + modified covariates)
# Egger's Regression RVE with Modified Covariates
#--------------------------------------------------

Egg_Sandwich_robu <- function(formula, dat, estimator, rho = 0.8) {
  
  suppressPackageStartupMessages(
    require(robumeta, quietly = TRUE, warn.conflicts = FALSE)
  )
  robu_model <- robu(formula, data = dat, studynum = umbrella, var.eff.size = vi, 
                     rho = rho, small = TRUE)
  
  robu_results <- with(robu_model, data.frame(estimator = estimator,
                                              est = as.numeric(robu_model$reg_table$b.r[1]), 
                                              se = as.numeric(robu_model$reg_table$SE[1]), 
                                              p_val = as.numeric(robu_model$reg_table$prob[1]), 
                                              p_nomissing = pt(as.numeric(robu_model$reg_table$t[2]), 
                                                               as.numeric(robu_model$reg_table$dfs[2]), 
                                                               lower.tail = FALSE)))
}



egg <- Egg_Sandwich_robu(formula = final_e_size ~ 1 + sda, dat = dat2, estimator = "Egger Sandwich Test")














###################################
########Egg Sandwich Full #########
###################################

# Egg_Sandwich_full <- function(dat, estimator, corr_impute = 0.8, fix_corr = TRUE) {
#   
#   suppressPackageStartupMessages(
#     require(nlme, quietly = TRUE, warn.conflicts = FALSE)
#   )
#   
#   if (all(table(dat$umbrella) == 1)) {
#     egg_fit <- nlme::lme(fixed = final_e_size ~ 1 + sda, 
#                          random = ~ 1 | umbrella, 
#                          weights = varFixed(~ vi),
#                          data = dat,
#                          control = nlme::lmeControl(sigma = 1, returnObject = TRUE, apVar = FALSE))  
#   } else {
#     
#     dat$var_bar <- with(dat, as.numeric(tapply(vi, umbrella, mean)[as.character(umbrella)]))
#     
#     egg_fit <- nlme::lme(fixed = final_e_size ~ 1 + sda, 
#                          random = ~ 1 | umbrella, 
#                          weights = varFixed(~ var_bar),
#                          correlation = corCompSymm(corr_impute, ~ 1 | umbrella, fixed = fix_corr),
#                          data = dat,
#                          control = nlme::lmeControl(sigma = 1, returnObject = TRUE, apVar = FALSE))
#   }
#   
#   sandwich_results <- clubSandwich::coef_test(egg_fit, vcov = "CR2")
#   
#   with(sandwich_results, data.frame(estimator = estimator,
#                                     est = beta[1],
#                                     se = SE[1], 
#                                     p_val = p_Satt[1], 
#                                     p_nomissing = pt(beta[2] / SE[2], df = df[2], lower.tail = FALSE)))
#   
# }
#egg2 <- Egg_Sandwich_full(dat = dat2, estimator = "Egger Sandwich-Full Model")












# effect_sizes_fluency <- effect_sizes_long_full %>%
#   filter(fluency == T)

# effect_sizes_fluency <- escalc(measure="ZCOR", ri=cor, ni=total_n, data=effect_sizes_fluency, slab=umbrella)
# 
# effect_sizes_fluency$reading_measure <- factor(effect_sizes_fluency$reading_measure)
# 
# 
# fluency_intercept <- robu(formula = final_e_size ~ 1, data = effect_sizes_fluency,
#                           studynum = umbrella, var.eff.size = vi)
# 
# dev.off()
# forest.robu(fluency_intercept, es.lab = "citation", study.lab = "umbrella",
#             "r" = flip_cor, "RAN_Type" = ran_type, "Reading Measure" = reading_measure, 
#             "Length" = timespan)
# effect_sizes_long_full$study_id <- 1:nrow(effect_sizes_long_full)



