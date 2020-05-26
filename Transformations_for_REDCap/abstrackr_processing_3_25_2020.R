rm(list=ls())
setwd("/Volumes/NortonLab/Longitudinal_RAN_Reading_Meta_Analysis/Meta_analysis_new_and_improved/REDCap_Imports/")

x <- read.csv("abstrackr_3_24_download.csv")

x <- x %>%
  filter(consensus == 1) %>%
  select(X.source..id, title, abstract, general.notes..mcweenysean2.)
x$microsoft_id <- x$X.source..id
x <- x %>%
  select(-X.source..id)

library(readxl)
year <- read_xlsx("/Volumes/NortonLab/Longitudinal_RAN_Reading_Meta_Analysis/Meta_analysis_new_and_improved/Abstract_screening/3rd_rd_abstracts_march_25_2020.xlsx")
year$authors <- year$Authors
year$id <- year$ID
year$year <- year$Year
year <- year %>%
  select(id, year, authors) %>%
  filter(id %in% x$microsoft_id)

x <- x %>%
  filter(microsoft_id %in% year$id)


x <- full_join(x, year, by = c("microsoft_id" = "id"))

only_author <- str_extract(x$authors, regex("[\\w]*$"))
first_author_last_name <- str_extract(x$authors, regex('[:alpha:]*(?=,)'))

author_last_name <- ifelse(is.na(first_author_last_name), only_author, first_author_last_name)
library(Hmisc)
author_last_name <- capitalize(author_last_name)


x$record_id <- paste0(author_last_name, ", ", x$year, "; ", str_extract(x$title, "^.{20}"), " [2]")

x <- x %>%
  select(record_id, microsoft_id,	title,	authors,	year, abstract, general.notes..mcweenysean2.)

names(x) <- c("record_id", "microsoft_id",	"title",	"authors",	"year", "abstract", "notes")

y <- read.csv("to_upload_3_2_2020.csv")

x <- x %>%
  filter(!record_id %in% y$record_id)


write.csv(x, "to_upload2_3_25_2020.csv", row.names = F)
