source("/Volumes/NortonLab/Longitudinal_RAN_Reading_Meta_Analysis/Meta_analysis_new_and_improved/R_Scripts/REDCap_sources/load_full_text_codes.R")

training_articles <- dat %>%
  filter(record_id %in% c("Bishop, 2006; Identifying a multiv [1]",
                          "Evans, 2006; Letter Names, Letter [1]",	
                          "Heath, 2004; Cost-effective predi (Good PA) [1]",
                          "Schatschneider, 2004; Kindergarten Predict [1]"))

cadence <- dat%>%
  filter(coder %in% c("CRB", "cb"))

june <- dat %>%
  filter(coder %in% c("YJC", "yjc"))

alice <- dat %>%
  filter(coder %in% c("aw", "AW"))

anu <- dat %>%
  filter(coder %in% c("ar", "AR"))

codee <- alice


compare_bishop <- rbind(codee[1,], training_articles[1,])
compare_evans <- rbind(codee[2,], training_articles[2,])
compare_heath <- rbind(codee[3,], training_articles[3,])
compare_schat <- rbind(codee[4,], training_articles[4,])

(agree_bishop <- compare_bishop[1,] == compare_bishop[2,])

(agree_evans <- compare_evans[1,] == compare_evans[2,])

(agree_heath <- compare_heath[1,] == compare_heath[2,])

(agree_schat <- compare_schat[1,] == compare_schat[2,])





compare_bishop <- compare_bishop %>%
  select(record_id, publication:latent_cor, -demographic_complete, -ran_describe, -information_location) %>%
  arrange(record_id) #%>%



long <- gather(compare_bishop, "variable", "agreement", 2:ncol(compare_bishop)) %>%
  arrange(record_id)


disagree <- long %>%
  filter(agreement == F)





