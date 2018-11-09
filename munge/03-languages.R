library("rvest")
library(tidyverse)

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
my.est = my.est %>% spread(l2,n)

len = nrow(languages)
f.con <- NULL
for (i in 1:len){
  tmp = matrix( 0, nrow = len, ncol = len)
  tmp[i, ] = tmp[i,] + 1
  tmp = rbind(tmp, diag(len))
  f.con = cbind(all, tmp)
}

f.rhs = c(languages$n2, languages$n1)
f.obj = rep(1, ncol(f.con))
f.dir <- rep("<=", 2*len)

tmp = diag(ncol(f.con))
f.con = rbind(f.con,tmp)
f.rhs = c(f.rhs, rep(0, ncol(f.con)))
f.dir <- c(f.dir, rep(">=", ncol(f.con)))


test = lp ("max", f.obj, f.con, f.dir, f.rhs)
test$solution

library(lpSolve)
#
# Set up problem: maximize
# x1 + 9 x2 + x3 subject to
# x1 + 2 x2 + 3 x3 <= 9
# 3 x1 + 2 x2 + 2 x3 <= 15
#
f.obj <- c(1, 9, 1)
f.con <- matrix (c(1, 2, 3, 3, 2, 2), nrow=2, byrow=TRUE)
f.dir <- c("<=", "<=")
f.rhs <- c(9, 15)
#
# Now run.
#
lp ("max", f.obj, f.con, f.dir, f.rhs)
## Not run: Success: the objective function is 40.5
lp ("max", f.obj, f.con, f.dir,