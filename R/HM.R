#' HM is function which return probability, that sequence is CpG island. If return>0 it isn't CpG island, if return>0 it is CpG island.
#' @details input is part of sequence, where we wanted to know, if it should be the CpG island
#' @export


HM<-function(sek){
  P<-matrix(c(0.18,0.17,0.16,0.08,0.27,0.37,0.34,0.36,0.43,0.27,0.37,0.38,0.12,0.19,0.13,0.18),4,4)
  M<-matrix(c(0.30,0.32,0.25,0.18,0.20,0.30,0.25,0.24,0.29,0.08,0.29,0.29,0.21,0.30,0.21,0.29),4,4)
  delka<-length(sek)
  prvky<-0

  for (i in 1:(delka-1)){
    if (sek[i]==DNAString('A')){
      prvni<-1;
    }
    else if (sek[i]==DNAString('C')){
      prvni<-2;
    }
    else if (sek[i]==DNAString('G')){
      prvni<-3;
    }
    else if (sek[i]==DNAString('T')){
      prvni<-4;
    }
    else{
      print('Neo?ek?van? znak')
      break
    }

    if (sek[i+1]==DNAString('A')){
      druhy<-1;
    }
    else if (sek[i+1]==DNAString('C')){
      druhy<-2;
    }
    else if (sek[i+1]==DNAString('G')){
      druhy<-3;
    }
    else if (sek[i+1]==DNAString('T')){
      druhy<-4;
    }
    else{
      print('Neo?ek?van? znak')
      break
    }
    prvky<-c(prvky,log(P[prvni,druhy]/M[prvni,druhy],10))
  }
  vysledek<-sum(prvky)
  return(vysledek)
}
