#' NextLeaf demonstrates how to jump from an L-mer a = (a1 a2 ··· aL) to the next L-mer in the progression.
#' @details a is position where we are; L is maximal level of the tree, k is number of options (number of alphabet)
#' @return new position where we are
#' @export

NextLeaf<-function(a,L,k){
  for (i in L:1){
    if (a[i]<k){
      a[i]<-a[i]+1
      return(a)
    }
    a[i]<-1
  }
  return(a)
}

