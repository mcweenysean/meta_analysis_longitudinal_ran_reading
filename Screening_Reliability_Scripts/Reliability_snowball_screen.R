library(tidyverse)
source("/Volumes/NortonLab/Longitudinal_RAN_Reading_Meta_Analysis/Meta_analysis_new_and_improved/R_Scripts/REDCap_sources/load_snowball_screens.R")

dat_og <- dat[seq(1, nrow(dat), 2),]
dat_og2 <- dat[seq(2, nrow(dat), 2),]

consensus <- dat$inclusion_exclusion == lag(dat$inclusion_exclusion)
consensus <- data.frame(consensus[-(seq(1, nrow(dat), 2))])
consensus <- data.frame(cbind(consensus = consensus[1:nrow(consensus),]), dat_og$record_id, dat_og$inclusion_exclusion, dat_og$coder_inc, dat_og2$record_id, dat_og2$inclusion_exclusion, dat_og2$coder_inc)


conflicts <- consensus %>%
  filter(consensus == F)

#write.csv(conflicts, "Conflicts_4_6_2020.csv")
#write.csv(conflicts, "Conflicts_4_13_2020.csv")
#write.csv(conflicts, "Conflicts_5_5_2020.csv")

complete_agreement <- consensus %>%
  filter(consensus == T)

include <- consensus %>%
  filter(dat_og.inclusion_exclusion == "Include" & dat_og2.inclusion_exclusion == "Include")


x2 <- dat_og %>%
  filter(inclusion_exclusion == "Include")




for.redcap <- dat_og %>%
  filter(record_id %in% include$dat_og.record_id)

for.redcap$record_id <- paste("[S]", for.redcap$record_id)
for.redcap$source_id <- for.redcap$microsoft_id

for.redcap <- for.redcap %>%
  select(record_id, source_id, title, authors, year, abstract, inclusion_exclusion,	coder_inc,	notes)
for.redcap$inclusion_exclusion <- 1
for.redcap$coder_inc <- ""

#write.csv(for.redcap, "/Volumes/NortonLab/Longitudinal_RAN_Reading_Meta_Analysis/Meta_analysis_new_and_improved/REDCap_Imports/import_41_snowballs_4_13_2020.csv", row.names = F)

for.redcap$record_id <- gsub("\\[1\\]", "[2]", for.redcap$record_id, perl = T)

#write.csv(for.redcap, "/Volumes/NortonLab/Longitudinal_RAN_Reading_Meta_Analysis/Meta_analysis_new_and_improved/REDCap_Imports/import_41_snowballs_rd2_4_13_2020.csv", row.names = F)



for.redcap <- dat_og %>%
  filter(record_id %in% new_include$dat_og.record_id)
for.redcap$record_id
for.redcap$source_id <- for.redcap$microsoft_id
for.redcap <- for.redcap %>%
  select(record_id, source_id, title, authors, year, abstract, inclusion_exclusion,	coder_inc,	notes)
for.redcap$inclusion_exclusion <- 1
for.redcap$coder_inc <- ""

#write.csv(for.redcap, "import_4_snowballs_5_8_2020.csv", row.names = F)

for.redcap$record_id <- gsub("\\[1\\]", "[2]", for.redcap$record_id, perl = T)

# write.csv(for.redcap, "import_4_snowballs2_5_8_2020.csv", row.names = F)
















# [1] "[S] Arnett, 2012; a cross lagged model [1]"        
# [2] "[S] Burgoyne, 2019; pattern understandin [1]"      
# [5] "[S] Byrne, 2009; genetic and environm [1]"         
# [6] "[S] Catts, 1991; early identification [1]"         
# [7] "[S] Catts, 1999; language basis of re [1]"         
# [8] "[S] Catts, 2002; a longitudinal inves [1]"         
# [9] "[S] Catts, 2016; early identification [1]"         
# [10] "[S] Chiu, 2018; the simple view of r [1]"          
# [11] "[S] Ellis, 1988; the early stages of  [1]"         
# [12] "[S] Elwer, 2014; early predictors of  [1]"         
# [14] "[S] Felton, 1990; phonological process [1]"        
# [15] "[S] Flynn, 2012; stability of special [1]"         
# [17] "[S] Gunn, 2010; evaluating the effec [1]"          
# [18] "[S] Hadley, 2016; piloting an early id [1]"        
# [19] "[S] Hecht, 2000; explaining social cl [1]"         
# [20] "[S] Hood, 2005; the role of temporal [1]"          
# [21] "[S] Little, 2012; a comparison of resp [1]"        
# [22] "[S] Menyuk, 1991; predicting reading p [1]"        
# [23] "[S] Missall, 2007; examination of the p [1]"       
# [24] "[S] Oconnor, 1999; prediction of readin [1]"       
# [25] "[S] Oconnor, 2000; increasing the inten [1]"       
# [26] "[S] Oslund, 2015; can curriculum embed [1]"        
# [27] "[S] Ozernovpalchik, 2019; the relationship bet [1]"
# [28] "[S] Phillips, 2009; predictive validity  [1]"      
# [29] "[S] Pike, 2012; a longitudinal exami [1]"          
# [30] "[S] Scanlon, 1996; prerequisite skills  [1]"       
# [31] "[S] Seward, 2009; evaluating the effec [1]"        
# [32] "[S] Shanahan, 2008; the effects of suppl [1]"      
# [33] "[S] Simmons, 2011; effects of supplemen [1]"       
# [34] "[S] Simmons, 2014; predictors of at ris [1]"       
# [35] "[S] Smith, 1996; an examination of th [1]"         
# [36] "[S] Snowling, 2019; developmental outcom [1]"      
# [37] "[S] Torgesen, 1994; longitudinal studies [1]"      
# [38] "[S] Vellutino, 2008; using response to ki [1]"     
# [39] "[S] Wolf, 1984; naming reading and t [1]"          
# [40] "[S] Wolter, 2011; initial mental graph [1]"        
# [41] "[S] Zuk, 2019; multifactorial pathw [1]" 


