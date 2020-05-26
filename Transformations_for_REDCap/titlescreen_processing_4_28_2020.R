rm(list=ls())
library(stringr)
library(tidyverse)
setwd("/Volumes/NortonLab/Longitudinal_RAN_Reading_Meta_Analysis/Meta_analysis_new_and_improved/REDCap_Imports/")

x <- readxl::read_xlsx("title_screened_import_redcap_4_28_2020.xlsx")

# x <- x %>%
#   filter(consensus == 1) %>%
#   select(X.source..id, title, abstract, general.notes..mcweenysean2.)
# x$microsoft_id <- x$X.source..id
# x <- x %>%
#   select(-X.source..id)
# 
# library(readxl)
# year <- read_xlsx("/Volumes/NortonLab/Longitudinal_RAN_Reading_Meta_Analysis/Meta_analysis_new_and_improved/Abstract_screening/3rd_rd_abstracts_march_25_2020.xlsx")
# year$authors <- year$Authors
# year$id <- year$ID
# year$year <- year$Year
# year <- year %>%
#   select(id, year, authors) %>%
#   filter(id %in% x$microsoft_id)

# x <- x %>%
#   filter(microsoft_id %in% year$id)
# 
# 
# x <- full_join(x, year, by = c("microsoft_id" = "id"))

only_author <- str_extract(x$authors, regex("[\\w]*$"))
first_author_last_name <- str_extract(x$authors, regex('[:alpha:]*(?=,)'))

author_last_name <- ifelse(is.na(first_author_last_name), only_author, first_author_last_name)
library(Hmisc)
author_last_name <- capitalize(author_last_name)


x$record_id <- paste0("[S] ", author_last_name, ", ", x$year, "; ", str_extract(x$title, "^.{20}"), " [2]")
x$record_id <- gsub("'", "", x$record_id)

x$notes <- ""

write.csv(x, "to_upload2_4_28_2020.csv", row.names = F)
