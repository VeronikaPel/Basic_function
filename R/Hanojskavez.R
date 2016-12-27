#' Hanojskavez is function which displays solution Tower of Hanoi.
#' @details n is number of disks; zkoliku is place from are disks moving, nakolik is place, where should be every disks in right order.
#' @export
#'


Hanojskavez<-function(n,zkoliku,nakolik){
  if (n==1){
    cat("presun z koliku", zkoliku ,"na kolik", nakolik,'\n')
    return()
  }
  else {
    volnykolik<-6-zkoliku-nakolik
    Hanojskavez(n-1,zkoliku,volnykolik)
    cat("presun z koliku", zkoliku ,"na kolik" ,nakolik,'\n')
    Hanojskavez(n-1,volnykolik,nakolik)
  }
}
