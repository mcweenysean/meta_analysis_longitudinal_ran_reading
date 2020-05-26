source("/Volumes/NortonLab/Longitudinal_RAN_Reading_Meta_Analysis/Meta_analysis_new_and_improved/R_Scripts/REDCap_sources/load_full_screen.R")

dat_og <- dat[seq(1, 832, 2),]

consensus <- dat$inclusion_exclusion == lag(dat$inclusion_exclusion)
consensus <- data.frame(consensus[-(seq(1, 832, 2))])
consensus <- data.frame(cbind(consensus = consensus[1:416,]), dat_og$record_id)

consensus$record_id2 <- gsub("[1]", "[2]", consensus$dat_og.record_id, fixed = 2)
consensus$dat_og.record_id <- as.character(consensus$dat_og.record_id)


conflicts <- consensus %>%
  filter(consensus == F)

complete_agreement <- consensus %>%
  filter(consensus == T)

#####
conf1 <- dat %>%
  filter(record_id %in% conflicts$dat_og.record_id)
conf2 <- dat %>%
  filter(record_id %in% conflicts$record_id2)
#####
conflict <- full_join(conf1, conf2) %>%
  arrange(record_id)

View(conflict)
rm(conf1, conf2)

