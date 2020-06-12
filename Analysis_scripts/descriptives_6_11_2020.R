source("/Volumes/NortonLab/Longitudinal_RAN_Reading_Meta_Analysis/Meta_analysis_new_and_improved/R_Scripts/Analysis_scripts/Effect_size_reshaping_1_27_2020.R")
library(psych)

summary_stats <- effect_sizes_long_full %>%
  group_by(umbrella) %>%
  summarise(max_per_study = max(total_n), age = mean(age_months), age_sd = mean(age_months_sd), 
            time_between = mean(timelapse), ran_items = mean(ran_item_num))
(n <- sum(summary_stats$max_per_study))

(desc <- describe(summary_stats[,2:ncol(summary_stats)], na.rm = T))
paste0("The sample (n = ", n, ") was drawn from ", max(desc$n), " independent samples across 68 papers. For studies that reported an initial age, the mean initial age was ", round(desc$mean[2], 2),  " months with a mean SD of ", round(desc$mean[3], 2), ". The mean interval between initial and final timepoints was ", round(desc$mean[4], 2), " months, which is consistent with our prioritization of the Grade 2 timepoint.")


risk_desc <- effect_sizes_long_full %>% group_by(umbrella, risk) %>% 
  count()

# Descriptives for categoricals
##### 
n_per <- function(var, val){
    var <- enquo(var)
    x <- effect_sizes_long_full %>% group_by(umbrella) %>%
      filter(!!var == val)
  x1 <- x %>% summarise(max_per_study = max(total_n))
  sum(x1$max_per_study)
}

# starting timepoint
(k_n <- n_per(init_text, "K"))
(prek_n <- n_per(init_text, "Pre-K"))
(mixed_n <- n_per(init_text, "Mixed"))

(g1n <- n_per(final_text, "Grade 1"))
(g2n <- n_per(final_text, "Grade 2"))
(g3n <- n_per(final_text, "Grade 3"))
(g4n <- n_per(final_text, "Grade 4"))


rm(k_n, prek_n, mixed_n, g1n, g2n, g3n, g4n)
#####
