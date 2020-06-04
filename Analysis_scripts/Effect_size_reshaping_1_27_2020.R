source("/Volumes/NortonLab/Longitudinal_RAN_Reading_Meta_Analysis/Meta_analysis_new_and_improved/R_Scripts/REDCap_sources/load_full_text_codes.R")

#filter for useable data from first coder (excludes training articles)
effect_sizes <- dat %>%
  filter(nousabledata == "Yes" & measures_complete == "Complete") %>%
  filter(str_detect(record_id, pattern = "\\[1\\]$")) 

#This function collapses the 44 composite checkbox reading measures to 1 variable for
# each composite. However, it will fuck up your data types so keep the original data so you can join 
#this with the original data

composite.list <- function(x, nms){
  names(x) <- nms
  #there are 2 composite vars because one is ordered according to alphabetized var names (1,10,11..2,20,21)
  composite_vars <- c('WRMT Word ID', 'GORT Passage Score', 'GORT Rate', 'KTEA Reading Comprehension', 'KTEA Reading Decoding', 'BASIS Reading Comprehension', 'TOWRE Sight Word Efficiency (SWE)', 'TOWRE Phonemic Decoding Efficiency (PDE)','NARA Reading Comprehension (Accuracy)', 'NARA Reading Comprehension (Content)',  'YARC Word Reading', 'WRMT Word Attack', 'YARC Early Word Reading', 'Schonell Single Word Graded Reading Test',  'Primary Reading Test', 'PIAT', 'Gray Silent Reading Tests', 'Oral and Written Language Scales', 'QRI Fluency', 'QRI Comprehension', 'TOSWRF', 'TORC', 'WRMT Passage Comprehension',  'PAL', 'Gates-MacGinitie Reading Test', 'YARC Passage Reading', 'Unstandardized Picture-Word Matching Task', 'WRAT Reading', 'DIBELS Oral Reading Fluency', 'AIMSWeb Mazes', 'WJ Word Attack', 'NA', 'NA', 'WRMT Word Comprehension',  "NA", "NA", "WIAT Basic Reading", "California Achievement Test - Reading", "Other", 'WRMT Letter-Word ID', 'WJ Letter-Word ID', 'WJ Sentence Reading Fluency', 'WJ Passage Comprehension', 'Stanford Achievement Test Reading Comprehension')
  #composite_vars2 <-c('WRMT Word ID', 'WRMT Word Attack', 'WRMT Passage Comprehension', 'WRMT Word Comprehension', 'WRMT Letter-Word ID', 'WJ Letter-Word ID', 'WJ Sentence Reading Fluency', 'WJ Passage Comprehension', 'Stanford Achievement Test Reading Comprehension', 'GORT Passage Score', 'GORT Rate', 'KTEA Reading Comprehension', 'KTEA Reading Decoding', 'BASIS Reading Comprehension', 'TOWRE Sight Word Efficiency (SWE)', 'TOWRE Phonemic Decoding Efficiency (PDE)','NARA Reading Comprehension (Accuracy)', 'NARA Reading Comprehension (Content)',  'YARC Word Reading', 'YARC Early Word Reading', 'Schonell Single Word Graded Reading Test',  'Primary Reading Test', 'PIAT', 'Gray Silent Reading Tests', 'Oral and Written Language Scales', 'QRI Fluency', 'QRI Comprehension', 'TOSWRF', 'TORC', 'PAL', 'Gates-MacGinitie Reading Test', 'YARC Passage Reading', 'Unstandardized Picture-Word Matching Task', 'WRAT Reading', 'DIBELS Oral Reading Fluency', 'AIMSWeb Mazes', 'WJ Word Attack', 'NA', 'NA', "NA", "NA", "WIAT Basic Reading", "California Achievement Test - Reading", "Other")
  
   x <- enframe(x) %>%
     spread(key = "name", value = "value")
 
  comp <- x %>%
    select(comprehension_composite_types___1:comprehension_composite_types___9)
  vec <- which(comp == "Checked")
  vec_names <- composite_vars[vec]
  vec_names <- paste(vec_names, collapse=", ")
  x$comprehension_composite <- paste("Reading Comprehension Composite:", vec_names)
  x$comprehension_composite <- str_replace(x$comprehension_composite, "Other", x$comp_composite_types_other) 
  
  decoding <- x %>%
    select(decoding_composite_types___1:decoding_composite_types___9)
  vec <- which(decoding == "Checked")
  vec_names <- composite_vars[vec]
  vec_names <- paste(vec_names, collapse=", ")
  x$decoding_composite <- paste("Decoding Composite:", vec_names)
  x$decoding_composite <- str_replace(x$decoding_composite, "Other", as.character(x$decoding_composite_types_other))
  
  
  fluency <- x %>%
    select(fluency_composite_types___1:fluency_composite_types___9)
  vec <- which(fluency == "Checked")
  vec_names <- composite_vars[vec]
  vec_names <- paste(vec_names, collapse=", ")
  x$fluency_composite <- paste("Fluency Composite:", vec_names)
  x$fluency_composite <- str_replace(x$fluency_composite, "Other", as.character(x$fluency_composite_types_other))
  
  
  gen_reading <- x %>%
    select(gen_reading_composite_types___1:gen_reading_composite_types___9)
  vec <- which(gen_reading == "Checked")
  vec_names <- composite_vars[vec]
  vec_names <- paste(vec_names, collapse=", ")
  x$gen_reading_composite <- paste("General Reading Composite:", vec_names)
  x$gen_reading_composite <- str_replace(x$gen_reading_composite, "Other", as.character(x$gen_reading_composite_types_other))
  
  
  ran_composite_vars <- c("Colors", "Objects", "Letters", "Numbers")
  ran_composite_alpha <- x %>%
    select(ran_composite_types___1:ran_composite_types___4)
  vec <- which(ran_composite_alpha == "Checked")
  vec_names <- ran_composite_vars[vec]
  vec_names <- paste(vec_names, collapse=", ")
  x$ran_composite <- paste("RAN Composite:", vec_names)
  
  
  ran_composite_alpha_2 <- x %>%
    select(ran_composite_types_2___1:ran_composite_types_2___4)
  vec <- which(ran_composite_alpha_2 == "Checked")
  vec_names <- ran_composite_vars[vec]
  vec_names <- paste(vec_names, collapse=", ")
  x$ran_composite_2 <- paste("RAN Composite:", vec_names)
  
  
  return(x)
}

#iterate the composite.list over each row. This was nastier than anticipated
#due to needing named vectors, and conversions between base and tidyverse structures
esize2 <- apply(effect_sizes, MARGIN = 1, FUN = composite.list, nms = names(dat))

#from list to tibble
esize3 <- reduce(esize2, bind_rows)

#extract new vars, along with ID to join by
esize3 <- esize3 %>%
  select(record_id, comprehension_composite:ran_composite_2)

#now you can overwrite dat
effect_sizes <- full_join(effect_sizes, esize3, by = "record_id")

#get rid of the checklist vars
effect_sizes <- effect_sizes %>%
  select(-comprehension_composite_types___1:-comprehension_composite_types___44,
         -decoding_composite_types___1:-decoding_composite_types___44,
         -fluency_composite_types___1:-fluency_composite_types___44,
         -gen_reading_composite_types___1:-gen_reading_composite_types___44,
         -ran_composite_types___1:-ran_composite_types___4,
         -ran_composite_types_2___1:-ran_composite_types_2___4)

#create NAs for composite measures that don't exist
effect_sizes$comprehension_composite <- na_if(effect_sizes$comprehension_composite, "Reading Comprehension Composite: ") 
effect_sizes$decoding_composite <- na_if(effect_sizes$decoding_composite, "Decoding Composite: ") 
effect_sizes$fluency_composite <- na_if(effect_sizes$fluency_composite, "Fluency Composite: ") 
effect_sizes$gen_reading_composite <- na_if(effect_sizes$gen_reading_composite, "General Reading Composite: ") 
effect_sizes$ran_composite <- na_if(effect_sizes$ran_composite, "RAN Composite: ") 
effect_sizes$ran_composite_2 <- na_if(effect_sizes$ran_composite_2, "RAN Composite: ") 



#####
#find number of unique titles for prisma
# es <- effect_sizes %>%
#   select(title, record_id)
# 
# unique(es$title)
#write.csv(effect_sizes$title, "to_search_2_25_2020.csv")
#write.csv(effect_sizes$title, "zotero_refs_4_8_2020.csv")

#####
## RAN measures vars cleaning 
effect_sizes$ran_array_num[effect_sizes$ran_array_num=="n/a"] <- NA
effect_sizes$ran_array_num[is.na(effect_sizes$ran_array_num)] <- 1
effect_sizes$ran_array_num <- as.numeric(effect_sizes$ran_array_num)


effect_sizes$ran_item_num[effect_sizes$ran_item_num=="n/a"] <- NA
effect_sizes$ran_item_num <- as.numeric(effect_sizes$ran_item_num)


effect_sizes$ran_item_unique[effect_sizes$ran_item_unique=="n/a"] <- NA
effect_sizes$ran_item_unique <- as.numeric(effect_sizes$ran_item_unique)

effect_sizes$total_ran_items <- effect_sizes$ran_array_num * effect_sizes$ran_item_num
#deal with composites multiplication later 



#Transform all ages to months
effect_sizes$age_months <- ifelse(effect_sizes$age_months_years == "Years", effect_sizes$age_reported_inital*12, effect_sizes$age_reported_inital*1)

effect_sizes$age_reported_sd <- as.numeric(effect_sizes$age_reported_sd)
effect_sizes$age_months_sd <- ifelse(effect_sizes$age_months_years == "Years", effect_sizes$age_reported_sd*12, effect_sizes$age_reported_sd*1)


#building citation from author and year variables
effect_sizes$citation <- ifelse(str_count(effect_sizes$authors, ";") == 0, 
                   (paste(word(effect_sizes$authors, 1), substr(effect_sizes$year, start = 1, stop = 4))), 
                   ifelse(str_count(effect_sizes$authors, ";") == 1, paste0(str_extract(pattern = "[^,]*" , effect_sizes$authors), " & ", str_extract(pattern = regex("(?<=; ).*(?=,)"), effect_sizes$authors), ", ", substr(effect_sizes$year, start = 1, stop = 4)),
                          paste(str_extract(pattern = "[^,]*" , effect_sizes$authors), "et al.,", substr(effect_sizes$year, start = 1, stop = 4))))
  
effect_sizes <- effect_sizes %>%
  select(record_id, citation, umbrella, total_n, 
         initial_timepoint_to_code, gradeschool_timepoint_to_code,
         ran_colors_initial:ran_composite2_initial, ran_score, ran_alt, total_ran_items, ran_std, ran_item_num, ran_item_unique, ran_array_num,
         reading_measure_1:reading_measure_11_other,
         e_size_ran_c_rm1:measures_complete, comprehension_composite:ran_composite_2,
         age_months, age_months_sd, sample_type, publication) %>%
  mutate(flip = ran_score %in% c("Standard Score", "Rate (Items/Time)", "Raw Score (Time) but the correlation is an absolute value", "Not Reported/Unclear (positive correlation)"))

#change "flip" from boolean to 1 or -1 to multiply effect sizes by
effect_sizes$flip <- ifelse(effect_sizes$flip == T, -1, 1)



effect_sizes$initial_timepoint <- ifelse(effect_sizes$initial_timepoint_to_code == "Preschool/Reception (UK) Fall", 0, 
                                  ifelse(effect_sizes$initial_timepoint_to_code == "Preschool/Reception (UK) Spring", 6,
                                  ifelse(effect_sizes$initial_timepoint_to_code == "Kindergarten/(UK/AUS Year 1) Fall", 12,
                                  ifelse(effect_sizes$initial_timepoint_to_code == "Kindergarten/(UK/AUS Year 1) Spring", 18,
                                  ifelse(effect_sizes$initial_timepoint_to_code == "Kindergarten/(UK/AUS Year 1) (time unclear)", 15,
                                  ifelse(effect_sizes$initial_timepoint_to_code == "Preschool/Reception (UK) (time unclear)", 3,
                                  ifelse(effect_sizes$initial_timepoint_to_code == "Mixed Preschool and Kindergarten (time unclear)", 9, no = -9999
                                         )))))))

effect_sizes$init_text <- ifelse(effect_sizes$initial_timepoint_to_code %in% c("Preschool/Reception (UK) Fall", "Preschool/Reception (UK) Spring", "Preschool/Reception (UK) (time unclear)"), "Pre-K", 
                          ifelse(effect_sizes$initial_timepoint_to_code %in% c("Kindergarten/(UK/AUS Year 1) Fall", "Kindergarten/(UK/AUS Year 1) Spring", "Kindergarten/(UK/AUS Year 1) (time unclear)"), "K",
                          ifelse(effect_sizes$initial_timepoint_to_code == "Mixed Preschool and Kindergarten (time unclear)", "Mixed", no = -9999
                          )))
  


effect_sizes$final_timepoint <- ifelse(effect_sizes$gradeschool_timepoint_to_code == "Fall Grade 1 (US) / Fall Grade 2 (UK)", 24, 
                                  ifelse(effect_sizes$gradeschool_timepoint_to_code == "Spring Grade 1 (US) / Spring Grade 2 (UK)", 30,
                                  ifelse(effect_sizes$gradeschool_timepoint_to_code == "Fall Grade 2 (US) / Fall Grade 3 (UK)", 36,
                                  ifelse(effect_sizes$gradeschool_timepoint_to_code == "Spring Grade 2 (US) / Spring Grade 3 (UK)", 42,
                                  ifelse(effect_sizes$gradeschool_timepoint_to_code == 'Fall Grade 3 (US) / Fall Grade 4 (UK)', 48,
                                  ifelse(effect_sizes$gradeschool_timepoint_to_code == 'Spring Grade 3 (US) / Spring Grade 4 (UK)', 54,
                                  ifelse(effect_sizes$gradeschool_timepoint_to_code == 'Fall Grade 4 (US) / Fall Grade 5 (UK)', 60,
                                  ifelse(effect_sizes$gradeschool_timepoint_to_code == 'Spring Grade 4 (US) / Spring Grade 5 (UK)' , 66,
                                  ifelse(effect_sizes$gradeschool_timepoint_to_code == 'Fall Grade 5 (US) / Fall Grade 6 (UK)' , 72,
                                  ifelse(effect_sizes$gradeschool_timepoint_to_code == 'Spring Grade 5 (US) / Spring Grade 6 (UK)', 78,
                                  ifelse(effect_sizes$gradeschool_timepoint_to_code == 'Grade 1 (NOS) (US) / Year 2 (UK)', 27,
                                  ifelse(effect_sizes$gradeschool_timepoint_to_code == 'Grade 2 (NOS) (US) / Year 3 (UK)', 39,  
                                  ifelse(effect_sizes$gradeschool_timepoint_to_code == 'Grade 3 (NOS) (US) / Year 4 (UK)', 51,
                                  ifelse(effect_sizes$gradeschool_timepoint_to_code == 'Grade 4 (NOS) (US) / Year 5 (UK)', 63,  
                                  ifelse(effect_sizes$gradeschool_timepoint_to_code == 'Grade 5 (NOS) (US) / Year 6 (UK)', 75, no = -99
                                          )))))))))))))))


effect_sizes$final_text <- ifelse(effect_sizes$gradeschool_timepoint_to_code %in% c("Fall Grade 1 (US) / Fall Grade 2 (UK)", "Spring Grade 1 (US) / Spring Grade 2 (UK)", 'Grade 1 (NOS) (US) / Year 2 (UK)'), "Grade 1", 
                           ifelse(effect_sizes$gradeschool_timepoint_to_code %in% c("Fall Grade 2 (US) / Fall Grade 3 (UK)", "Spring Grade 2 (US) / Spring Grade 3 (UK)", 'Grade 2 (NOS) (US) / Year 3 (UK)'), "Grade 2",
                           ifelse(effect_sizes$gradeschool_timepoint_to_code %in% c("Fall Grade 3 (US) / Fall Grade 4 (UK)", "Spring Grade 3 (US) / Spring Grade 4 (UK)", 'Grade 3 (NOS) (US) / Year 4 (UK)'), "Grade 3",
                           ifelse(effect_sizes$gradeschool_timepoint_to_code %in% c("Fall Grade 4 (US) / Fall Grade 5 (UK)", "Spring Grade 4 (US) / Spring Grade 5 (UK)", 'Grade 4 (NOS) (US) / Year 5 (UK)'), "Grade 4",
                           ifelse(effect_sizes$gradeschool_timepoint_to_code %in% c("Fall Grade 5 (US) / Fall Grade 6 (UK)", "Spring Grade 5 (US) / Spring Grade 6 (UK)", 'Grade 5 (NOS) (US) / Year 6 (UK)'), "Grade 5", no = -99
                           )))))



effect_sizes$timelapse <- effect_sizes$final_timepoint - effect_sizes$initial_timepoint



effect_sizes$timespan <- paste0(effect_sizes$init_text, "-to-", effect_sizes$final_text)


effect_sizes$risk <- ifelse(effect_sizes$sample_type %in% c("School-based sample", "Control Group (typical/good PA/No-Risk Group)", "Gifted"), "Low Risk",
                            ifelse(effect_sizes$sample_type == "Oversampled for dyslexia, but has lots of typicals as well", "Medium Risk", 
                            ifelse(effect_sizes$sample_type %in% c("Dyslexia Group (retroactive classification)", "Only dyslexia-risk"), "High Risk", NA
                                   )))




#change data into long format, but conventional methods of gather don't work because each study
#has differing numbers of effect sizes. The solution is to gather for each of the possible 
#lengths and then join them together. probably should've written a function for this, but I didn't.

######
gather.cor.length <- function(x, measure_num) {
  varname <- paste0("reading_measure_", measure_num)
  varname2 <- paste0("reading_measure_", measure_num, "_other")
  
  x <- x %>%
    gather(ran_type, cor, paste0("e_size_ran_c_rm", measure_num):paste0("e_size_ran_composite2_rm", measure_num)) %>%
    filter(cor != "") %>%
    mutate(reading_measure := !!rlang::sym(varname)) %>%
    mutate(reading_measure_other := !!rlang::sym(varname2))
  
  x$reading_measure_other <- as.character(x$reading_measure_other)
  return(x)
}

effect_sizes_long_1cor <- gather.cor.length(effect_sizes, 1)
effect_sizes_long_2cor <- gather.cor.length(effect_sizes, 2)
effect_sizes_long_3cor <- gather.cor.length(effect_sizes, 3)
effect_sizes_long_4cor <- gather.cor.length(effect_sizes, 4)
effect_sizes_long_5cor <- gather.cor.length(effect_sizes, 5)
effect_sizes_long_6cor <- gather.cor.length(effect_sizes, 6)
effect_sizes_long_7cor <- gather.cor.length(effect_sizes, 7)
effect_sizes_long_8cor <- gather.cor.length(effect_sizes, 8)
effect_sizes_long_9cor <- gather.cor.length(effect_sizes, 9)
effect_sizes_long_10cor <- gather.cor.length(effect_sizes, 10)
effect_sizes_long_11cor <- gather.cor.length(effect_sizes, 11)



# effect_sizes_long_5cor <- effect_sizes %>%
#   gather(ran_type, cor, e_size_ran_c_rm5:e_size_ran_composite2_rm5) %>%
#   filter(cor != "") %>%
#   mutate(reading_measure = reading_measure_5) %>%
#   mutate(reading_measure_other = reading_measure_5_other)
# 
# effect_sizes_long_7cor <- effect_sizes %>%
#   gather(ran_type, cor, e_size_ran_c_rm7:e_size_ran_composite2_rm7) %>%
#   filter(cor != "") %>%
#   mutate(reading_measure = reading_measure_7) #%>%
#   #mutate(reading_measure_other = reading_measure_7_other)
# 
# effect_sizes_long_9cor <- effect_sizes %>%
#   gather(ran_type, cor, e_size_ran_c_rm9:e_size_ran_composite2_rm9) %>%
#   filter(cor != "") %>%
#   mutate(reading_measure = reading_measure_9)# %>%
#  # mutate(reading_measure_other = reading_measure_9_other)
# 
# effect_sizes_long_10cor <- effect_sizes %>%
#   gather(ran_type, cor, e_size_ran_c_rm10:e_size_ran_composite2_rm10) %>%
#   filter(cor != "") %>%
#   mutate(reading_measure = reading_measure_10) #%>%
#   #mutate(reading_measure_other = reading_measure_10_other)
# 
# effect_sizes_long_11cor <- effect_sizes %>%
#   gather(ran_type, cor, e_size_ran_c_rm11:e_size_ran_composite2_rm11) %>%
#   filter(cor != "") %>%
#   mutate(reading_measure = reading_measure_11) #%>%
#   #mutate(reading_measure_other = reading_measure_11_other)





effect_sizes_long <- full_join(effect_sizes_long_1cor, effect_sizes_long_2cor)
effect_sizes_long <- full_join(effect_sizes_long, effect_sizes_long_3cor)
effect_sizes_long <- full_join(effect_sizes_long, effect_sizes_long_4cor)
effect_sizes_long <- full_join(effect_sizes_long, effect_sizes_long_5cor)
effect_sizes_long <- full_join(effect_sizes_long, effect_sizes_long_6cor)
effect_sizes_long <- full_join(effect_sizes_long, effect_sizes_long_7cor)
effect_sizes_long <- full_join(effect_sizes_long, effect_sizes_long_8cor)
effect_sizes_long <- full_join(effect_sizes_long, effect_sizes_long_9cor)
effect_sizes_long <- full_join(effect_sizes_long, effect_sizes_long_10cor)
effect_sizes_long <- full_join(effect_sizes_long, effect_sizes_long_11cor)



#make RAN variables less ugly, more human readable for tables
effect_sizes_long$ran_type <- gsub("e_size_ran_n.*", "Numbers", x = effect_sizes_long$ran_type)
effect_sizes_long$ran_type <- gsub("e_size_ran_c_.*", "Colors", x = effect_sizes_long$ran_type)
effect_sizes_long$ran_type <- gsub("e_size_ran_o.*", "Objects", x = effect_sizes_long$ran_type)
effect_sizes_long$ran_type <- gsub("e_size_ran_l.*", "Letters", x = effect_sizes_long$ran_type)
effect_sizes_long$ran_type <- gsub("e_size_ran_a.*", "Colored Animals", x = effect_sizes_long$ran_type)
effect_sizes_long$ran_type <- gsub(".*e_size_ran_composite_.*", "Composite", x = effect_sizes_long$ran_type)
effect_sizes_long$ran_type <- gsub(".*e_size_ran_composite2_.*", "Composite 2", x = effect_sizes_long$ran_type)



effect_sizes_long$ran_type <- ifelse(effect_sizes_long$ran_type == "Composite", effect_sizes_long$ran_composite,
                              ifelse(effect_sizes_long$ran_type == "Composite 2", effect_sizes_long$ran_composite_2, effect_sizes_long$ran_type))

effect_sizes_long$ran_type <- ifelse(effect_sizes_long$ran_type == "RAN Composite: Objects", "Objects", effect_sizes_long$ran_type)


effect_sizes_long$total_ran_items_mult <- ifelse(!str_detect(effect_sizes_long$ran_type, "Composite:"), 1, 
                                                 ifelse(str_detect(effect_sizes_long$ran_type, pattern = regex(",.*,.*,")), 4, 
                                                        ifelse(str_detect(effect_sizes_long$ran_type, pattern = regex(",.*,")), 3, 
                                                               ifelse(str_detect(effect_sizes_long$ran_type, pattern = regex(",")), 2, 1))))

effect_sizes_long$total_ran_items <- effect_sizes_long$total_ran_items * effect_sizes_long$total_ran_items_mult 


effect_sizes_long$published <- ifelse(effect_sizes_long$publication %in% c("Other unpublished data", "Unpublished dissertation or thesis"), "Unpublished", "Published")
effect_sizes_long$emailed <- ifelse(effect_sizes_long$publication == "Other unpublished data", "Emailed", "Not emailed")

# effect_sizes_long$reading_measure2 <- ifelse(str_detect(effect_sizes_long$reading_measure, "Composite"), effect_sizes_long$)
# 
# effect_sizes_long$reading_measure2 <- ifelse(str_detect(effect_sizes_long$reading_measure, "Composite"), effect_sizes)



#clean up workspace
rm(esize2, esize3, effect_sizes_long_1cor, effect_sizes_long_2cor, effect_sizes_long_3cor, effect_sizes_long_4cor, effect_sizes_long_5cor, effect_sizes_long_6cor, effect_sizes_long_7cor, effect_sizes_long_8cor, effect_sizes_long_9cor, effect_sizes_long_10cor, effect_sizes_long_11cor)

#####
library(psych)

#select all variables that may be included in final analyses
effect_sizes_long_full <- effect_sizes_long %>%
  select(record_id, citation, umbrella, total_n, published, emailed, timelapse, timespan, initial_timepoint, final_timepoint, 
         latent_cor, ran_score, ran_item_num, ran_item_unique, ran_std, ran_array_num, ran_alt, total_ran_items, 
         reading_measure, reading_measure_other, comprehension_composite, decoding_composite, fluency_composite, gen_reading_composite, ran_type, age_months, age_months_sd, sample_type, risk,
         cor, flip) %>%
  arrange(record_id)
  

##

#Wolf, 1986 Single Word Reading Task flip - last observation
effect_sizes_long_full$cor[which(effect_sizes_long_full$reading_measure_other == "Single Word Reading Task")] <- (effect_sizes_long_full$cor[which(effect_sizes_long_full$reading_measure_other == "Single Word Reading Task")] * -1)

#Georgiou 2012 total time
effect_sizes_long_full$cor[which(effect_sizes_long_full$reading_measure_other == "GORT total time")] <- (effect_sizes_long_full$cor[which(effect_sizes_long_full$reading_measure_other == "GORT total time")] * -1)


#z transform
effect_sizes_long_full <- effect_sizes_long_full %>%
  mutate(std_z = fisherz(as.numeric(cor)))
 

#combine flip and z transform
effect_sizes_long_full <- effect_sizes_long_full %>%
  mutate(final_e_size = std_z*flip)

#### reading measure other replace reading measure
effect_sizes_long_full$reading_measure <- as.character(effect_sizes_long_full$reading_measure)
effect_sizes_long_full$reading_measure_other <- as.character(effect_sizes_long_full$reading_measure_other)
effect_sizes_long_full$reading_measure <- ifelse(effect_sizes_long_full$reading_measure == "Other", effect_sizes_long_full$reading_measure_other, effect_sizes_long_full$reading_measure)

#composite list replaces "Decoding Composite"
effect_sizes_long_full$reading_measure <- case_when(effect_sizes_long_full$reading_measure == "Comprehension Composite" ~ effect_sizes_long_full$comprehension_composite,
                                                    effect_sizes_long_full$reading_measure == "Decoding Composite" ~ effect_sizes_long_full$decoding_composite,
                                                    effect_sizes_long_full$reading_measure == "General Reading Composite" ~ effect_sizes_long_full$gen_reading_composite,
                                                    effect_sizes_long_full$reading_measure == "Fluency Composite" ~ effect_sizes_long_full$fluency_composite,
                                                    !effect_sizes_long_full$reading_measure %in% c("Fluency Composite", "General Reading Composite", "Decoding Composite", "Comprehension Composite") ~ effect_sizes_long_full$reading_measure)


##umbrella studies
effect_sizes_long_full$umbrella <- as.character(effect_sizes_long_full$umbrella)
effect_sizes_long_full$umbrella <- ifelse(is.na(effect_sizes_long_full$umbrella), effect_sizes_long_full$citation, effect_sizes_long_full$umbrella)


#classifying RAN measures - issues with strange composites with alpha and non-alphanumeric measures
effect_sizes_long_full$ran_alphanumeric_pre <- effect_sizes_long_full$ran_type
effect_sizes_long_full$ran_alphanumeric_pre <- na_if(effect_sizes_long_full$ran_alphanumeric_pre, "RAN Composite: Colors, Objects, Letters, Numbers")
effect_sizes_long_full$ran_alphanumeric_pre <- na_if(effect_sizes_long_full$ran_alphanumeric_pre, "RAN Composite: Colors, Objects, Numbers") 
effect_sizes_long_full$ran_alphanumeric_pre <- na_if(effect_sizes_long_full$ran_alphanumeric_pre, "RAN Composite: Objects, Letters, Numbers")

effect_sizes_long_full$ran_alphanumeric <- ifelse(is.na(effect_sizes_long_full$ran_alphanumeric_pre), NA, 
                                                  ifelse(effect_sizes_long_full$ran_alphanumeric_pre %in% c("Letters", "Numbers", "RAN Composite: Letters, Numbers"), "Alphanumeric", "Non-alphanumeric"))

effect_sizes_long_full$ran_colors_objects <- ifelse(effect_sizes_long_full$ran_type == "Colors", "Colors", 
                                                    ifelse(effect_sizes_long_full$ran_type == "Objects", "Objects", NA))

x <- readxl::read_xlsx("/Volumes/NortonLab/Longitudinal_RAN_Reading_Meta_Analysis/Meta_analysis_new_and_improved/Reading Measures/Reading Measures Meta Analysis.xlsx")
x <- x %>% select(-notes, -notes2, -described)
x[x == "NA"] <- NA

effect_sizes_long_full <- full_join(effect_sizes_long_full, x, by = "reading_measure")


  
#####
#classifying reading measures
#(all_reading_measures <- levels(factor(effect_sizes_long_full$reading_measure)))
#time vs. no time
# effect_sizes_long_full$time <- ifelse(effect_sizes_long_full$reading_measure %in% c("TOWRE Sight Word Efficiency (SWE)", "TOWRE Phonemic Decoding Efficiency (PDE)",
#                                                                               "Words per Second", "QRI Fluency", "Florida Assessment of Instruction for Reading: Oral Reading Fluency",
#                                                                               "Fluency Composite: TOWRE Sight Word Efficiency (SWE), DIBELS Oral Reading Fluency", "Fluency Composite: TOWRE Sight Word Efficiency (SWE), TOWRE Phonemic Decoding Efficiency (PDE)",
#                                                                               "GORT Accuracy", "GORT Comprehension", "GORT Fluency", "GORT ORI", "GORT Passage Score", "GORT Rate", "GORT total time", "GORT: Composite Fluency Score"), "Timed", 
#                                       ifelse(effect_sizes_long_full$reading_measure %in% c("General Reading Composite: GORT Passage Score, GORT Rate, KTEA Reading Comprehension, KTEA Reading Decoding", "General Reading Composite: GORT Passage Score, GORT Rate, KTEA Reading Comprehension, KTEA Reading Decoding, KTEA Spelling"
#                                                                                            ), NA, "Untimed"))
# 
#                                       
#                                       
#                                       
# #true fluency vs. timed vs. untimed
# 
# 
# #comprehension vs. the world
# effect_sizes_long_full$comprehension <- (effect_sizes_long_full$reading_measure %in% c("Gates-MacGinitie Reading Test")
#                                          | str_detect(effect_sizes_long_full$reading_measure, "Comprehension"))
# 
# # effect_sizes_long_full %>%
# #   select(reading_measure, time, comprehension)
# 
# 
# effect_sizes_long_full$nonword <- ifelse(effect_sizes_long_full$reading_measure %in% c("WRMT Word Attack", "WJ Word Attack", "TOWRE Phonemic Decoding Efficiency (PDE)", "Nonword Reading"), "Nonword",
#                                          ifelse(effect_sizes_long_full$reading_measure %in% c("WJ Letter-Word ID", "WRMT Word ID", "WJ Word ID", "British Abilities Scale (BAS)", "Word Recognition: Burns/Roe Informal Reading Inventory"), "Single Word", NA))
# 
# 
# 
# effect_sizes_long_full$fluency1 <- ifelse(effect_sizes_long_full$reading_measure %in% c("QRI Fluency", "Florida Assessment of Instruction for Reading: Oral Reading Fluency") | str_detect(effect_sizes_long_full$reading_measure, "GORT"), "Fluency", "Not Fluency")
# 
# effect_sizes_long_full$fluency2 <- ifelse(effect_sizes_long_full$reading_measure %in% c("GORT comprehension", "GORT Comprehension", "GORT Fluency", "GORT ORI", "GORT Passage Score", "GORT: Composite Fluency Score", "QRI Fluency", "Florida Assessment of Instruction for Reading: Oral Reading Fluency"), "Fluency",
#                                           ifelse(effect_sizes_long_full$reading_measure %in% c("GORT Rate", "GORT total time", "GORT Accuracy"), NA, "Not Fluency"))
# 
# 
# effect_sizes_long_full$efficiency <- ifelse(effect_sizes_long_full$reading_measure %in% c("TOWRE Sight Word Efficiency (SWE)", "TOWRE Phonemic Decoding Efficiency (PDE)"), "TOWRE",
#                                             ifelse(effect_sizes_long_full$reading_measure %in% c("WRMT Word ID, WRMT Word Attack", "WJ Word Attack", "WJ Letter-Word ID"), "Woodcock", NA))
# 
# 
# singleword <- c("British Abilities Scale (BAS)", "Decoding Composite: WRAT Reading, WJ Word Attack, WJ Letter-Word ID", "Decoding Composite: WRMT Word ID, WRMT Word Attack", "Decoding Composite: WRMT Word ID, TOWRE Sight Word Efficiency (SWE), TOWRE Phonemic Decoding Efficiency (PDE), WRMT Word Attack", 
#                 "Early Word Reading Test", "Experimental word attack (nonsense word decoding)", "Fluency Composite: TOWRE Sight Word Efficiency (SWE), TOWRE Phonemic Decoding Efficiency (PDE)",
#                 "Nonword Reading", "SAT Reading Vocabulary", "Schonell Reading", "Silent Comprehension", "Single Word Reading Task", "Single Word Reading Test", "TOWRE Phonemic Decoding Efficiency (PDE)", "TOWRE Sight Word Efficiency (SWE)",
#                 "Unstandardized Picture-Word Matching Task", "WDRB Letter-Word ID", "WDRB Word Attack", "WJ Letter-Word ID", "WJ Word Attack", "Word Reading", "Word Recognition: Burns/Roe Informal Reading Inventory", "WRMT Letter-Word ID", "WRMT Word Attack", "WRMT Word ID", "YARC Early Word Reading",
#                 "WRMT Word Comprehension", "WIAT Basic Reading") #| str_detect("GORT")
# passage <- c("California Achievement Test - Reading",  "Florida Assessment of Instruction for Reading: Oral Reading Fluency", "Gates-MacGinitie Reading Test", "MAP",
#              "Oral Reading Fluency", "QRI Comprehension", "QRI Fluency", "Reading Rate", "Stanford Achievement Test Reading Comprehension", "WJ Passage Comprehension", 
#              "Words in Text Passage",  "WRMT Passage Comprehension", "YARC Passage Reading", "Words per Second"
#              ) #| str_detect("Reading Comprehension Composite:")
# both <- c("Decoding Composite: WRMT Word ID, WRMT Passage Comprehension", "Fluency Composite: TOWRE Sight Word Efficiency (SWE), DIBELS Oral Reading Fluency", "General Reading Composite: GORT Passage Score, GORT Rate, KTEA Reading Comprehension, KTEA Reading Decoding", "General Reading Composite: GORT Passage Score, GORT Rate, KTEA Reading Comprehension, KTEA Reading Decoding, KTEA Spelling" )
# 
# 
# 
# 
# effect_sizes_long_full$connected <- ifelse(effect_sizes_long_full$reading_measure %in% singleword | str_detect(effect_sizes_long_full$reading_measure, "GORT"), "Single Word", 
#                                            ifelse(effect_sizes_long_full$reading_measure %in% passage | str_detect(effect_sizes_long_full$reading_measure, "Reading Comprehension Composite"), "Connected Text", 
#                                                   ifelse(effect_sizes_long_full$reading_measure %in% both, NA, NA)))
# # 
# [1] "British Abilities Scale (BAS)"  - a single word reading task (Carroll 2016)                                
# [2] "California Achievement Test - Reading" (Felton 1992) The reading portion of the California Achievement Test comprises two subtests— vocabulary and comprehension—which are presented in a multiple choice format.                                                                                      
# [3] "Decoding Composite: WRAT Reading, WJ Word Attack, WJ Letter-Word ID"                                                            
# [4] "Decoding Composite: WRMT Word ID, TOWRE Sight Word Efficiency (SWE), TOWRE Phonemic Decoding Efficiency (PDE), WRMT Word Attack"
# [5] "Decoding Composite: WRMT Word ID, WRMT Passage Comprehension"                                                                   
# [6] "Decoding Composite: WRMT Word ID, WRMT Word Attack"                                                                             
# [7] "Early Word Reading Test" - Single word reading (Snowling 2019)                                                                                             
# [8] "Experimental word attack (nonsense word decoding)"                                                                              
# [9] "Florida Assessment of Instruction for Reading: Oral Reading Fluency"                                                            
# [10] "Fluency Composite: TOWRE Sight Word Efficiency (SWE), DIBELS Oral Reading Fluency"                                              
# [11] "Fluency Composite: TOWRE Sight Word Efficiency (SWE), TOWRE Phonemic Decoding Efficiency (PDE)"                                 
# [12] "Gates-MacGinitie Reading Test"                                                                                                  
# [13] "General Reading Composite: GORT Passage Score, GORT Rate, KTEA Reading Comprehension, KTEA Reading Decoding"                    
# [14] "General Reading Composite: GORT Passage Score, GORT Rate, KTEA Reading Comprehension, KTEA Reading Decoding, KTEA Spelling"     
# [15] "GORT Accuracy"                                                                                                                  
# [16] "GORT Comprehension"                                                                                                             
# [17] "GORT Fluency"                                                                                                                   
# [18] "GORT ORI"                                                                                                                       
# [19] "GORT Passage Score"                                                                                                             
# [20] "GORT Rate"                                                                                                                      
# [21] "GORT total time"                                                                                                                
# [22] "GORT: Composite Fluency Score"                                                                                                  
# [23] "MAP" - Catts 2016 EI: Children read short narrative or expository passages via a computer and answer multiple-choice questions that require literal, inferential, or evaluative understanding of the text. Some test items also tap word meaning.                                                 
# [24] "Nonword Reading"                                                                                                                
# [25] "Oral Comprehension"                                                                                                             
# [26] "Oral Reading Fluency"                                                                                                           
# [27] "QRI Comprehension"                                                                                                              
# [28] "QRI Fluency"                                                                                                                    
# [29] "Reading Comprehension Composite: AIMSWeb Mazes, WJ Passage Comprehension"                                                       
# [30] "Reading Comprehension Composite: GORT Passage Score, WRMT Passage Comprehension, Diagnostic Achievement Battery "               
# [31] "Reading Comprehension Composite: The Reading Comprehension Measure, Measures of Academic Progress: Reading (MAP)"               
# [32] "Reading Comprehension Composite: WRMT Passage Comprehension, GORT Comprehension; Diagnostic Achievement Battery "               
# [33] "Reading Rate"                                                                                                                   
# [34] "SAT Reading Vocabulary"                                                                                                         
# [35] "Schonell Reading" - single word reading                                                                                                      
# [36] "Silent Comprehension"                                                                                                           
# [37] "Single Word Reading Task"                                                                                                       
# [38] "Single Word Reading Test"                                                                                                       
# [39] "Stanford Achievement Test Reading Comprehension"                                                                                
# [40] "TOWRE Phonemic Decoding Efficiency (PDE)"                                                                                       
# [41] "TOWRE Sight Word Efficiency (SWE)"                                                                                              
# [42] "Unstandardized Picture-Word Matching Task"                                                                                      
# [43] "WDRB Letter-Word ID"                                                                                                            
# [44] "WDRB Word Attack"                                                                                                               
# [45] "WIAT Basic Reading"                                                                                                             
# [46] "WJ Letter-Word ID"                                                                                                              
# [47] "WJ Passage Comprehension"                                                                                                       
# [48] "WJ Word Attack"                                                                                                                 
# [49] "Word Reading"                                                                                                                   
# [50] "Word Recognition: Burns/Roe Informal Reading Inventory"                                                                         
# [51] "Words in Text Passage"                                                                                                          
# [52] "Words per Second" - Uhry 2002, informal passage                                                                                                 
# [53] "WRMT Letter-Word ID"                                                                                                             
# [54] "WRMT Passage Comprehension"                                                                                                     
# [55] "WRMT Word Attack"                                                                                                               
# [56] "WRMT Word Comprehension"                                                                                                        
# [57] "WRMT Word ID"                                                                                                                   
# [58] "YARC Early Word Reading"                                                                                                        
# [59] "YARC Passage Reading"  
