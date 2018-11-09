library("rvest")
library(tidyverse)

format.all = function(tbl){
  names(tbl) = c("Topic", "Subtopic", "Wikidata", "Estimate", "Shortcuts")
  tbl = tbl %>% filter(complete.cases(.), !grepl("Total", Topic))
  tbl$Wikidata = as.numeric(gsub(",", "", tbl$Wikidata))
  tmp = str_split(tbl$Estimate, ",")
  tbl$Estimate = sapply(tmp, function(x){
    if (nchar(data.table::last(x)) > 3){
      x[length(x)] = substr(data.table::last(x),1,3)
    }
    x = as.numeric(paste(x, collapse=""))
    return(x)
  })
  return(tbl)
}
url <- "https://en.wikipedia.org/wiki/User:Emijrp/All_Human_Knowledge"
all <- url %>%
  read_html() %>%
  # html_nodes(xpath='//*[@id="mw-content-text"]/table[1]') %>%
  html_table(fill = TRUE) 

pos = sapply(all, dim)
pos = which(pos[2,] == 5)
all = all[pos]
all = lapply(all, format.all)
all = bind_rows(all)


l1 = strsplit(population$`L1 speakers`, "m")
l1 = lapply(l1, function(x) x[1])
l1 = as.numeric(gdata::trim(paste(l1)))


l2 = strsplit(population$`L2 speakers`, "m")
l2 = lapply(l2, function(x) x[1])
l2 = as.numeric(gdata::trim(paste(l2)))

vars = population[[5]]
