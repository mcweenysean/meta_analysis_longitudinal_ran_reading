if (.Platform$OS.type == "windows") {
  source("Z:\\Longitudinal_RAN_Reading_Meta_Analysis\\Meta_analysis_new_and_improved\\R_Scripts\\Analysis_scripts\\Effect_size_reshaping_1_27_2020.R")
} else {
  source("/Volumes/xxxx/Longitudinal_RAN_Reading_Meta_Analysis/Meta_analysis_new_and_improved/R_Scripts/Analysis_scripts/Effect_size_reshaping_1_27_2020.R")
}

############################## Analysis #################################
library(metafor)
library(robumeta)
#library(clubSandwich)

dat2 <- escalc(measure="ZCOR", ri=cor, ni=total_n, data=effect_sizes_long_full, slab=umbrella) 
dat2$n_char <- as.character(dat2$total_n)

dat2$sda <- sqrt(dat2$vi)

#####
##Table 3

all_intercept <- robu(formula = final_e_size ~ 1, data = dat2, modelweights = "CORR",
                      studynum = umbrella, var.eff.size = vi)

print(all_intercept)
fisherz2r(all_intercept[["b.r"]])

all_intercept_no_latent <- robu(formula = final_e_size ~ 1, data = dat2 %>% filter(latent_cor == "No"), modelweights = "CORR",
                                studynum = umbrella, var.eff.size = vi)

fisherz2r(all_intercept_no_latent[["b.r"]])





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
for_table_m_names <- c("all_intercept", "m_colors", "m_objects", "m_letters", "m_numbers", 'm_comprehension', "m_fluency",'m_single_word', 'm_real_word', 'm_nonword', "m_timed", "m_untimed", 'm_efficiency', 'm_accuracy')



table3 <- function(l, names){
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
output3 <- table3(for_table, for_table_m_names)
output3$reg.b.r <- round(fisherz2r(output3$reg.b.r), 2)
output3

#####
#Table 4



mod_unique <- robu(formula = final_e_size ~ 1 + ran_item_unique, data = dat2 %>% filter(!record_id == '[S] Hood, 2005; the role of temporal [1]'),
           studynum = umbrella, var.eff.size = vi)

mod_total_items <- robu(formula = final_e_size ~ 1 + total_ran_items, data = dat2 %>% filter(!record_id %in% c('Cirino, 2018; Longitudinal Predict [1]', 'Lachance, 2006; A longitudinal analy [1]')),
           studynum = umbrella, var.eff.size = vi)

mod_std <- robu(formula = final_e_size ~ 1 + ran_std, data = dat2,
           studynum = umbrella, var.eff.size = vi)

mod_age_assess <- robu(formula = final_e_size ~ 1 + initial_timepoint + final_timepoint, data = dat2,
            studynum = umbrella, var.eff.size = vi)

mod_alpha <- robu(formula = final_e_size ~ 1 + ran_alphanumeric, data = dat2,
                  studynum = umbrella, var.eff.size = vi)

mod_nonword <- robu(formula = final_e_size ~ 1 + nonword_single, data = dat2,
           studynum = umbrella, var.eff.size = vi)

mod_risk <- robu(formula = final_e_size ~ 1 + risk, data = dat2,
            studynum = umbrella, var.eff.size = vi)

mod_timed <- robu(formula = final_e_size ~ 1 + timed, data = dat2,
                  studynum = umbrella, var.eff.size = vi)

mod_efficiency <- robu(formula = final_e_size ~ 1 + efficiency, data = dat2,
                  studynum = umbrella, var.eff.size = vi)




for_table <- list(mod_unique, mod_total_items, mod_std, mod_age_assess, mod_alpha, mod_nonword, mod_timed, mod_efficiency, mod_risk)

table4 <- function(l){
  z <- data.frame()
  for(i in 1:length(l)){
    z1 <- data.frame(
      model = format(l[[i]][["ml"]]),
      n = l[[i]][["N"]],
      k = l[[i]][["M"]],
      i2 = l[[i]][["mod_info"]][["I.2"]],
      type = "model")
    z <- rbind(z, z1)  
  }
  
  zz <- data.frame()
  for(i in 1:length(l)){
    zz1 <- data.frame(
      model = format(l[[i]][["ml"]]),
      reg = l[[i]][["reg_table"]],
      type = "reg")
    zz <- rbind(zz, zz1)
  }
  zzz <- full_join(z, zz, by = c("model", "type"))
  df <- zzz[order(zzz$model),]
  
  #round all numeric columns
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  df[,nums] <- round(df[,nums], digits = 2)
  df$CI <- paste0("[", df$reg.CI.L, " ", df$reg.CI.U, "]")
  df <- df %>%
    select(-type, -reg.CI.L, -reg.CI.U, -reg.sig)
  return(df)
}

output4 <- table4(for_table)
output4


#####

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



robu(formula = final_e_size ~ 1 + sda, data = dat2,
     studynum = umbrella, var.eff.size = vi)


#####
#power
library(metapower)
mpower(.4, k = 60, sample_size = 176, es_type = "r")


#But one option may be to extract the degrees of freedom found in the analysis for the covariate of interest 
#(which will take into account the uneven k, n, etc) and then use these as the number of studies in a traditional MA power analysis 
#(e.g., using the package you mention). For example, if you have 20 studies but the df are 5.7, you might just say the sample size is 6 studies 
#(since these df are kind of the ‘effective sample size’)

fisherz2r(.5)
fisherz2r(.4)
fisherz2r(.3)

es <- c(0.46211718, 0.379949)
es3 <- c(0.46211718, 0.379949, 0.2913126)


subgroup_power(n_groups = 2, effect_sizes = es, sample_size = 176, k = 5, es_type = "r")
subgroup_power(n_groups = 2, effect_sizes = es, sample_size = 176, k = 11, es_type = "r")
subgroup_power(n_groups = 2, effect_sizes = es, sample_size = 176, k = 24, es_type = "r")
subgroup_power(n_groups = 2, effect_sizes = es, sample_size = 176, k = 20, es_type = "r")
subgroup_power(n_groups = 2, effect_sizes = es, sample_size = 176, k = 22, es_type = "r")
subgroup_power(n_groups = 2, effect_sizes = es, sample_size = 176, k = 37, es_type = "r")
subgroup_power(n_groups = 3, effect_sizes = es3, sample_size = 177, k = 12, es_type = "r")


