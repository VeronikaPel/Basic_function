#' NextVertex function moves down to the next lower level and explores that subtree of a.
#' @details a is position where we are;i is level where we are ;L is maximal level of the tree, k is number of options (number of alphabet)
#' @return new position where we are and level where we are
#' @export


NextVertex<-function(a,i,L,k){
  if (i<L){
    a[i+1]<-1
    return(list(a,i+1))
  }
  else{
    for (j in L:1){
      if (a[j]<k){
        a[j]<-a[j]+1
        return(list(a,j))
      }
    }
  }
  return(list(a,0))
}
