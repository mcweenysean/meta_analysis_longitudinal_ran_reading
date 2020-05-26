rm(list=ls())
source("/Volumes/NortonLab/Longitudinal_RAN_Reading_Meta_Analysis/Meta_analysis_new_and_improved/R_Scripts/REDCap_sources/load_full_screen.R")

dat_og <- dat[seq(1, 844, 2),]

dat_og %>% 
  group_by(inclusion_exclusion) %>%
  summarise(n_per_exclude = length(inclusion_exclusion))

dat_og %>%
  filter(inclusion_exclusion == "Include") %>%
  select(record_id)




# 
# import <- dat_og %>%
#   filter(record_id %in% c("Wiens, 2005; Sound-symbol learnin [1]", "Snowling, 2011; Identification of ch [1]", "Hoard, 2007; Mathematical cogniti [1]"))
# 
# import <- import %>%
#   select(-inclusion_complete)
# 
# import$coder_inc <- ""
# import$inclusion_exclusion <-1
# 
# 
# 
# write.csv(import, "missed_first_time_full_text_code_import_5_13_2020.csv", row.names = F)
