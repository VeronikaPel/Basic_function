#' Function LCP_vypocet return vector, where value lcp[i] indicates length of the longest common prefix of the suffixes inexed by suffix[i] and suffix[i+1]
#' @export

LCP_vypocet<-function(slovo){
  n<-length(slovo)
  vysledky<-suffix_array(slovo)
  ISA<-vysledky[[4]]
  SA<-vysledky[[3]]


  LCP<-c(0,ncol=(n+1),nrow=1)
  LCP[1]<--1
  LCP[n+1]<--1
  l<-0
  slovo<-c(slovo,DNAString('+'))

  for (i in 1:n){
    j<-ISA[i]
    if (j>1){
      k<-SA[j-1]
      while (slovo[k+l]==slovo[i+l]){
        l<-l+1
      }
      LCP[j]<-l
      l<-max(c((l-1),0))
    }
  }
  return(LCP)
}



