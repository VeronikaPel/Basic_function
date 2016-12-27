nesetridene<-function(a){
  a<-as.integer(a)
  delka<-length(a)
  vzestup<-as.integer(0)
  vektor<-as.integer(0)
  while (identical(a[1:(vzestup+1)],vektor)){
    vzestup<-vzestup+1
    vektor<-c(0:vzestup)
  }
  return(vzestup+1)
}
