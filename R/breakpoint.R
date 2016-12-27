#' Function breakpoint shows steps, how is vector sorting with using breakpoint method.
#' @description  input a is unsorted vector
#' @export

breakpoint<-function(a){
  delka<-length(a)
  a<-c(0,a,delka+1)
  print(a)
  pom<-0:(delka+1)

  while (TRUE){
    vzest<-vzestsest(a)
    nuly<-a[vzest==0]
    if(length(nuly)!=0){
      if(length(nuly)==1){
        pozice<-nesetridene(a)
        a[(pozice+1):(delka+1)]<-a[(delka+1):(pozice+1)]
        print(a)
      }

      else {
        min_sest<-min(nuly)
        pozice_min<-which(min_sest==a)
        oblast_pom<-a[pozice_min:1]
        setridene<-nesetridene(a)
        a[setridene:pozice_min]<-a[pozice_min:setridene]
        print(a)
      }
    }

    else{
      pozice<-nesetridene(a)
      a[(pozice+1):(delka+1)]<-a[(delka+1):(pozice+1)]
      print(a)

    }

    if (identical(as.integer(a),pom)){
      a<-a[2:(delka+1)]
      print(a)
      break
    }
  }
}

