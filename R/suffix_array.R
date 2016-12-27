#' suffix_array is function giving every suffix, suffix array as string and in vector of order, finally invers suffyx array in vector order.
#' @details input is sequence; outputs: suffix conteins all posible suffixes, abeceda is an array of all suffixes of A in lexicographical order, poradi is vector of order abeceda, inverz is invers suffix array
#' @export

suffix_array<-function(slovo){
  delka<-length(slovo)
  suffix<-DNAStringSet(slovo)
  for (i in 1:(delka-1)){
    suffix<-c(suffix,DNAStringSet(slovo[(i+1):delka]))
  }

  abeceda<-sort(suffix)

  poradi<-matrix(0,nrow=1,ncol=delka)
  for (j in 1:delka){
    for (k in 1: delka){
      if (identical(suffix[[k]],abeceda[[j]])){
        poradi[j]<-k
      }

    }
  }

  inverz<-matrix(0,nrow=1,ncol=delka)
  for (z in 1:delka){
    for (y in 1:delka){
      if (z==poradi[y]){
        inverz[z]<-y
      }
    }
  }
  return(list(suffix,abeceda,poradi,inverz))
}
