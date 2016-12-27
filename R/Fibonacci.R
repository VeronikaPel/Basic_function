#' Function Fibonacci is funtion which takes the value of n-th element from Fibonacci numbers
#' @return n-th element from Fibonacci numbers
#' @export

Fibonacci<-function(n){
  F[1]<-1
  F[2]<-1
  for (i in 3:n){
    F[i]<-F[i-1]+F[i-2]
  }
  return(F[n])
}
