library("rvest")
library(tidyverse)
library(lpSolve)
library(goalprog)
get.solution = function(lp.soln){
  data.frame(expand.grid(l1 = languages$Language, l2 = languages$Language), n = lp.soln$solution)
}

format.speakers = function(l1){
  l1 = gsub("\\?", "0", l1)
  pos = !grepl("m", l1)
  l1[pos] = gsub(",", "0", l1[pos])
  l1[pos] = paste("0.", l1[pos], " m", sep = "")
  l1 = strsplit(l1, "m")
  l1 = lapply(l1, function(x) x[1])
  l1 = as.numeric(gdata::trim(paste(l1)))
  return(l1)
}
url <- "https://en.wikipedia.org/wiki/List_of_languages_by_total_number_of_speakers"
languages <- url %>%
  read_html() %>%
  html_table(fill = TRUE)
languages <- languages[[1]]

languages$n1 <- format.speakers(languages$`L1 speakers`)
languages$n2 <- format.speakers(languages$`L2 speakers`)
languages$pc = languages$n1/sum(languages$n1)
languages = languages %>% arrange(desc(n1))
plot(cumsum(languages$n1), type = 'l')
#languages = languages[1:10,]
my.est = expand.grid(l1 = languages$Language, l2 = languages$Language)
tmp = languages %>% rename(l1 = Language) %>% select(l1, n1, pc)
my.est = left_join(my.est, tmp)
tmp = languages %>% rename(l2 = Language) %>% select(l2, n2)
my.est = left_join(my.est, tmp)
pos = my.est$l1 == my.est$l2
my.est1 = my.est[!pos,]
my.est1 = my.est1 %>% group_by(l2) %>% mutate(pc = pc/sum(pc))
my.est1$est = round(my.est1$pc * my.est1$n2, 0)
my.est = my.est[pos, ] %>% select(l1, l2, n1) %>% rename(n = n1)
my.est1 = my.est1 %>% select(l1, l2, est) %>% rename(n = est) %>% 
  as.data.frame()
my.est = rbind(my.est, my.est1)


len = nrow(languages)
f.con <- NULL
for (i in 1:len){
  tmp = matrix( 0, nrow = len, ncol = len)
  tmp[i, ] = tmp[i,] + 1
  #tmp[i,ncol(tmp)] = - 1
  tmp = rbind(tmp, diag(len))
  f.con = cbind(f.con, tmp)
}

f.rhs = c(languages$n1, languages$n2+languages$n1)
remember = nrow(f.con)
#min native speakers
tmp = diag(ncol(f.con))[seq(1,ncol(f.con), by = len+1),]
f.con = rbind(f.con, tmp)
f.rhs = c(f.rhs, 0.5*languages$n1)

f.achievements = data.frame(objective = 1:nrow(f.con),
                            priority =  c(rep(2, remember), rep(1, nrow(f.con)-remember)),
                            p = c(rep(1, remember), rep(10000, nrow(f.con)-remember)),
                            n = c(rep(1, remember), rep(10000, nrow(f.con)-remember)))
# soln <- llgp( coefficients, targets, achievements )
soln <- llgp( f.con, f.rhs, f.achievements )

x = my.est
x$n = soln$out$x 
x = x %>% spread(l2,n)
x$rowsums = rowSums(x[,-1])
x = rbind(x, c("total", colSums(x[,-1])))
