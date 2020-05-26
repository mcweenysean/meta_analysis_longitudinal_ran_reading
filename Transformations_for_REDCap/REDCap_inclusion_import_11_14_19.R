setwd('/Users/seanmcweeny/Documents/meta_analysis/')
library(tidyverse)
library(stringr)

data_abstrackr <- read.csv("Abstrackr_11_13_19_final.csv") %>%
  select(X.source..id, consensus, tags, general.notes..mcweenysean2., general.notes..jinnie., general.notes..yjc1997.)

data_zotero <- read.csv("Title Screen - Include.csv")

data_include <- full_join(data_abstrackr, data_zotero, by = c("X.source..id" = "Key")) %>%
  filter(consensus == 1) %>%
  select(X.source..id,	Title,	Author,	Date,	Abstract.Note)

names(data_include) <- c("source_id",	'title',	'authors',	'year',	'abstract')

data_include$inclusion_exclusion <- rep('', nrow(data_include))
data_include$coder <- rep('', nrow(data_include))
data_include$notes <- rep('', nrow(data_include))
data_include$record_id <- paste0(word(data_include$authors, 1), " ", substr(data_include$year, 1, 4), "; ", substr(data_include$title, 1, 20), " [1]")
data_include$record_id <- gsub("'", "_", data_include$record_id)


data_include <- data_include[c("record_id", "source_id",	'title',	'authors',	'year',	'abstract')]


write.csv(data_include, file = "import_include_11_14_19.csv", row.names=FALSE)

