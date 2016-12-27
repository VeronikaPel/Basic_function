vzestsest<-function(a){
  delka<-length(a)
  pom<-matrix(0,ncol=delka,nrow=1)
  pom[1]<-1
  pom[delka]<-1
  for (i in 2:delka-1){
    if ((a[i]+1)==a[i+1]){
      pom[i]<-1
      pom[i+1]<-1
    }
    else{}

  }
  return(pom)

}
