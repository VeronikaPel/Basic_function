#' Function VratitMince takes vector which number of coins should be get back.
#' @details M is amount of money which should be get back; c is vector of value coins, which should be used
#' @return number of coins which should be get back
#' @export
#'
VratitMince<-function(M,c){
  d<-length(c)
  Mince<-matrix(0,nrow=1,ncol=d)
  zbytek<-floor(M)

  for (i in d:1){
    Mince[i]<-floor(zbytek/c[i])
    zbytek<-zbytek-Mince[i]*c[i]
  }

  soucet<-sum(c*Mince)
  if (M==soucet){
    return(Mince)
  }
  else{
    print("Chyba")
  }


}
