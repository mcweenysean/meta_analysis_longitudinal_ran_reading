rm(list=ls())
setwd("/Volumes/NortonLab/Longitudinal_RAN_Reading_Meta_Analysis/Meta_analysis_new_and_improved/Abstract_screening/")

x <- read.csv("to_process_from_abstrackr_3_2_2020.csv")

x <- x %>%
  filter(consensus == 1) %>%
  select(microsoft_id, title, abstract, general.notes..mcweenysean2.)

library(readxl)
year <- read_xlsx("2nd_rd_abstracts_feb_10_2020.xlsx")
year <- year %>%
  select(id, year, authors) %>%
  filter(id %in% x$microsoft_id)


x <- full_join(x, year, by = c("microsoft_id" = "id"))

only_author <- str_extract(x$authors, regex("[\\w]*$"))
first_author_last_name <- str_extract(x$authors, regex('[:alpha:]*(?=,)'))

author_last_name <- ifelse(is.na(first_author_last_name), only_author, first_author_last_name)
author_last_name <- capitalize(author_last_name)


x$record_id <- paste0(author_last_name, ", ", x$year, "; ", str_extract(x$title, "^.{20}"), " [1]")

x <- x %>%
  select(record_id, microsoft_id,	title,	authors,	year, abstract, general.notes..mcweenysean2.)

names(x) <- c("record_id", "microsoft_id",	"title",	"authors",	"year", "abstract", "notes")

write.csv(x, "to_upload_3_2_2020.csv", row.names = F)
