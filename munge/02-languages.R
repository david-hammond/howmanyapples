library("rvest")
library(tidyverse)
url <- "https://en.wikipedia.org/wiki/List_of_languages_by_total_number_of_speakers"
population <- url %>%
  read_html() %>%
  # html_nodes(xpath='//*[@id="mw-content-text"]/table[1]') %>%
  html_table(fill = TRUE)
population <- population[[1]]

l1 = strsplit(population$`L1 speakers`, "m")
l1 = lapply(l1, function(x) x[1])
l1 = as.numeric(gdata::trim(paste(l1)))


l2 = strsplit(population$`L2 speakers`, "m")
l2 = lapply(l2, function(x) x[1])
l2 = as.numeric(gdata::trim(paste(l2)))

vars = 