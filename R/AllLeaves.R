#' AllLeaves gives all choices of leaves in tree
#' @details  L is maximal level of the tree, k is number of options (number of alphabet)
#' @return Output shows all choices of leaves in tree
#' @export

AllLeaves<-function(L,k){
  a<-matrix(1,nrow=1,ncol=L)
  while(1){
    print(a)
    a<-NextLeaf(a,L,k)
    if (identical(a,matrix(1,nrow=1,ncol=L))){
      return()
    }
  }
}
