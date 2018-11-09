bytes <- function(size){
  switch(size,
         mega = 10^6,
         giga = 10^9,
         tera = 10^12,
         peta = 10^15)
}
peta <- function(){
  bytes("peta")
}
giga <- function(){
  bytes("giga")
}
peta <- function(){
  bytes("peta")
}
mega <- function(){
  bytes("mega")
}
tera <- function(){
  bythes("tera")
}