source("/Volumes/NortonLab/Longitudinal_RAN_Reading_Meta_Analysis/Meta_analysis_new_and_improved/R_Scripts/REDCap_sources/load_full_text_codes.R")

coder1 <- dat[str_detect(dat$record_id, ".\\[1]"),]
#coder1_jinnie <- coder1[coder1$coder == "JC",]
#coder1_scam <- coder1[coder1$coder == "SCAM",]
coder2 <- dat[str_detect(dat$record_id, ".\\[2]"),]

z <- data.frame()

for(i in 1:nrow(coder1)){
  reli <- data.frame(coder1[i,] == coder2[i,])
  reli$record_id <- coder1$record_id[i]
  reli$coder1 <- coder1$coder[i]
  reli$coder2 <- coder2$coder[i]
  z <- rbind(reli, z)
}

z <- z %>%
  select(record_id, coder1, coder2, publication:latent_cor, -demographic_complete, -ran_describe, -information_location, -where_reported) %>%
  arrange(record_id) #%>%
  #filter(str_detect(record_id, "McIlraith")) 

z2 <- z %>%
  filter(record_id %in% c("Inoue, 2018; Examining an Extende (Grade 1 - Dec) [1]", "Inoue, 2018; Examining an Extende (Grade 1 - Dec) [2]"))



long <- gather(z, "variable", "agreement", 4:ncol(z)) %>%
  arrange(record_id)


disagree <- long %>%
  filter(agreement == F)



