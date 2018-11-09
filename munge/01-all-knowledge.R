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
  #tbl$pc = mmin(1, tbl$Estimate/tbl$Wikidata)
  tbl$Wikidata[is.na(tbl$Wikidata)] = 0
  tbl$Estimate[is.na(tbl$Estimate)] = 0
  pos = tbl$Estimate < tbl$Wikidata 
  tbl$Estimate[pos] = tbl$Wikidata[pos]
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
documented = sum(all$Wikidata)/sum(all$Estimate)

