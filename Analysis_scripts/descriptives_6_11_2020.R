source("/Volumes/NortonLab/Longitudinal_RAN_Reading_Meta_Analysis/Meta_analysis_new_and_improved/R_Scripts/Analysis_scripts/Effect_size_reshaping_1_27_2020.R")
library(psych)

summary_stats <- effect_sizes_long_full %>%
  group_by(umbrella) %>%
  summarise(max_per_study = max(total_n), age = mean(age_months), age_sd = mean(age_months_sd), 
            time_between = mean(timelapse), ran_items = mean(total_ran_items), unique = mean(ran_item_unique))
(n <- sum(summary_stats$max_per_study))

(desc <- describe(summary_stats[,2:ncol(summary_stats)], na.rm = T))
paste0("The sample (n = ", n, ") was drawn from ", max(desc$n), " independent samples across 68 papers. For studies that reported an initial age, the mean initial age was ", round(desc$mean[2], 2),  " months with a mean SD of ", round(desc$mean[3], 2), ". The mean interval between initial and final timepoints was ", round(desc$mean[4], 2), " months, which is consistent with our prioritization of the Grade 2 timepoint.")


u <- effect_sizes_long_full %>%
  group_by(umbrella) %>%
  filter(!is.na(ran_item_unique)) %>%
  select(ran_item_unique)
mean(u$ran_item_unique)


# Descriptives for categoricals
##### 
n_per <- function(var, val){
  var <- enquo(var)
  x <- effect_sizes_long_full %>% group_by(umbrella) %>%
      filter(!!var == val)
  x1 <- x %>% summarise(max_per_study = max(total_n))
  N <- sum(x1$max_per_study)
  kk <- effect_sizes_long_full %>% group_by(umbrella, !!var) %>%
    tally()
  xx <- kk %>% filter(!!var == val) %>% select(n)
  nn <- c(xx$n)
  k <- sum(nn)
  return(c(nrow(xx), k, N))
}

# starting timepoint
n_per(init_text, "K")
n_per(init_text, "Pre-K")
n_per(init_text, "Mixed")

n_per(final_text, "Grade 1")
n_per(final_text, "Grade 2")
n_per(final_text, "Grade 3")
n_per(final_text, "Grade 4")


n_per(ran_std, "Yes")
n_per(ran_std, "No")

n_per(risk, "Low Risk")
n_per(risk, "Medium Risk")
n_per(risk, "High Risk")
n_per(ran_alphanumeric, "Non-alphanumeric")
n_per(ran_alphanumeric, "Alphanumeric")

n_per(ran_colors_objects, "Colors")
n_per(ran_colors_objects, "Objects")
n_per(ran_letters_numbers, "Letters")
n_per(ran_letters_numbers, "Numbers")

n_per(latent_cor, "Yes")
n_per(latent_cor, "No")

#####

xx <- effect_sizes_long_full %>% filter(!is.na(total_ran_items))


x1 <- x %>% summarise(max_per_study = max(total_n))
N <- sum(x1$max_per_study)


n_per_numeric <- function(var){
  var <- enquo(var)
  x <- effect_sizes_long_full %>% group_by(umbrella) %>%
    filter(!is.na(!!var))
  x1 <- x %>% summarise(max_per_study = max(total_n))
  n <- sum(x1$max_per_study)
  k <- nrow(x)
  N <- nrow(x1)
  # kk <- effect_sizes_long_full %>% group_by(umbrella) %>%
  #   tally()
  # xx <- kk %>% filter(!is.na(!!var)) %>% select(n)
  # nn <- c(xx$n)
  # k <- sum(nn)
  return(c(N, k, n))
}

n_per_numeric(total_ran_items)
n_per_numeric(ran_item_unique)


effect_sizes_long_full %>% group_by(title) %>% 
  slice(1) %>% select(umbrella, title)
  
  


