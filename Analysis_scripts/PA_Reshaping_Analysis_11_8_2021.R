if (.Platform$OS.type == "windows") {
  source("Z:\\Longitudinal_RAN_Reading_Meta_Analysis\\Meta_analysis_new_and_improved\\R_Scripts\\Analysis_scripts\\Effect_size_reshaping_1_27_2020.R")
} else {
  source("/Volumes/NortonLab/Longitudinal_RAN_Reading_Meta_Analysis/Meta_analysis_new_and_improved/R_Scripts/Analysis_scripts/Effect_size_reshaping_1_27_2020.R")
}
rm(x, effect_sizes_long)



#filter for useable data from first coder (excludes training articles)
pa_effect_sizes <- dat %>%
  filter((nousabledata == "Yes" | nousabledata == "No code-able effects, but authors emailed data") & cor_matrix_complete == "Complete") %>%
  filter(str_detect(record_id, pattern = "\\[1\\]$")) %>%
  select(record_id, reading_measures_n, ran_colors_initial:ran_composite_initial, ran_composite2_initial, pa_measures_n:e_size_pa_5_rm11)

pa_effect_sizes$ran_colors_initial <- ifelse(pa_effect_sizes$ran_colors_initial=="Yes",1,0)
pa_effect_sizes$ran_colored_animals_initial <- ifelse(pa_effect_sizes$ran_colored_animals_initial=="Yes",1,0)
pa_effect_sizes$ran_objects_initial <- ifelse(pa_effect_sizes$ran_objects_initial=="Yes",1,0)
pa_effect_sizes$ran_numbers_initial <- ifelse(pa_effect_sizes$ran_numbers_initial=="Yes",1,0)
pa_effect_sizes$ran_letters_initial <- ifelse(pa_effect_sizes$ran_letters_initial=="Yes",1,0)
pa_effect_sizes$ran_composite_initial <- ifelse(pa_effect_sizes$ran_composite_initial=="Yes",1,0)
pa_effect_sizes$ran_composite2_initial <- ifelse(pa_effect_sizes$ran_composite2_initial=="Yes",1,0)


pa_effect_sizes$ran_measures_n <- rowSums(pa_effect_sizes[,c(3:9)])

pa_effect_sizes <- pa_effect_sizes %>% select(-ran_colors_initial:-ran_composite2_initial)

pa_effect_sizes$ran_measures_n * pa_effect_sizes$reading_measures_n * pa_effect_sizes$pa_measures_n

test <- pa_effect_sizes %>% pivot_longer(cols = c(e_size_ran_c_pa_1:e_size_ran_composite2_pa_1, e_size_ran_c_pa_2:e_size_ran_composite2_pa_2, e_size_ran_c_pa_3:e_size_ran_composite2_pa_3, e_size_ran_c_pa_4:e_size_ran_composite2_pa_4, e_size_ran_c_pa_5:e_size_ran_composite2_pa_5), names_to = 'measures_used', values_to = 'ran_pa_cor_val')

test <- test %>% filter(!is.na(ran_pa_cor_val)) %>% separate(measures_used, "_pa_", into = c('ran_measure', 'pa_measure'))


test$ran_measure <- gsub("e_size_ran_n$", "Numbers", x = test$ran_measure)
test$ran_measure <- gsub("e_size_ran_c$", "Colors", x = test$ran_measure)
test$ran_measure <- gsub("e_size_ran_o$", "Objects", x = test$ran_measure)
test$ran_measure <- gsub("e_size_ran_l$", "Letters", x = test$ran_measure)
test$ran_measure <- gsub("e_size_ran_a$", "Colored Animals", x = test$ran_measure)
test$ran_measure <- gsub("e_size_ran_composite$", "Composite", x = test$ran_measure)
test$ran_measure <- gsub("e_size_ran_composite2$", "Composite 2", x = test$ran_measure)

test$pa_measure_1 <- as.character(test$pa_measure_1)
test$pa_measure_2 <- as.character(test$pa_measure_2)
test$pa_measure_3 <- as.character(test$pa_measure_3)
test$pa_measure_4 <- as.character(test$pa_measure_4)
test$pa_measure_5 <- as.character(test$pa_measure_5)


test$pa_measure <- ifelse(test$pa_measure == "1", test$pa_measure_1, 
                          ifelse(test$pa_measure == "2", test$pa_measure_2,
                                 ifelse(test$pa_measure == "3", test$pa_measure_3,
                                        ifelse(test$pa_measure == "4", test$pa_measure_4,
                                               ifelse(test$pa_measure == "5", test$pa_measure_5,"Throw Error")))))




test2 <- test %>% pivot_longer(cols = c(e_size_pa_1_rm1:e_size_pa_1_rm11, e_size_pa_2_rm1:e_size_pa_2_rm11, e_size_pa_3_rm1:e_size_pa_3_rm11, e_size_pa_4_rm1:e_size_pa_4_rm11, e_size_pa_5_rm1:e_size_pa_5_rm11), names_to = 'measures_used_2', values_to = 'pa_rm_cor_val')

test2 <- test2 %>% filter(!is.na(pa_rm_cor_val)) %>% separate(measures_used_2, "_rm", into = c('x_pa_measure', 'reading_measure'))



test2$x_pa_measure <- ifelse(test2$x_pa_measure == "e_size_pa_1", test2$pa_measure_1, 
                          ifelse(test2$x_pa_measure == "e_size_pa_2", test2$pa_measure_2,
                                 ifelse(test2$x_pa_measure == "e_size_pa_3", test2$pa_measure_3,
                                        ifelse(test2$x_pa_measure == "e_size_pa_4", test2$pa_measure_4,
                                               ifelse(test2$x_pa_measure == "e_size_pa_5", test2$pa_measure_5,"Throw Error")))))


test2 <- test2 %>% filter(pa_measure == x_pa_measure) 


test_rm <- effect_sizes_long_full %>% select(record_id, ran_type, reading_measure, cor)


j <- left_join(test2, test_rm, by = "record_id")


z <- test_rm %>% group_by(record_id)%>%
        mutate(rm_count_test, 1:nrow())

#test2$reading_measure <- ifelse(test2$reading_measure == "1", )


