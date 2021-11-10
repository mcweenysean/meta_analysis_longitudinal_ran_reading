library(tidyverse)
x <- read.csv("Z:\\Longitudinal_RAN_Reading_Meta_Analysis\\Meta_analysis_new_and_improved\\Abstract_screening\\abstrackr_downloads\\Abstrackr_11_13_19_final.csv")


x$mcweenysean2
x$jinnie
x$yjc1997

x$mcweenysean2 <- ifelse(x$mcweenysean2 == "o", NA, x$mcweenysean2)
x$jinnie <- ifelse(x$jinnie == "o", NA, x$jinnie)
x$yjc1997 <- ifelse(x$yjc1997 == "o", NA, x$yjc1997)

x <- x %>% select(mcweenysean2, jinnie, yjc1997, consensus)

x$agree_sm_jinnie <- x$mcweenysean2 == x$jinnie
x$agree_sm_june <- x$mcweenysean2 == x$yjc1997
x$agree_june_jinnie <-x$jinnie == x$yjc1997

x %>% count(agree_sm_jinnie)
x %>% count(agree_sm_june)
x %>% count(agree_june_jinnie)            

(116 + 136 + 66)/(707 + 764 + 441 + 116 + 136 + 66)
