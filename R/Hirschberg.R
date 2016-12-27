#' Hirschberg function return align sequences. This align is made by Hirschberg algorithm.
#' @export

Hirschberg <- function(X,Y,Align,match,mismatch,gap){
  Z = Align[[1]] #inicializace prvniho radku zarovnani, typ DNAStringSet
  W = Align[[2]] #inicializace druheho radku zarovnani

  if (length(X)==0 && length(Y)!=0)      #delka X je nula
  {
    for (i in 1:length(Y) )  #podle poctu znaku v Y
    {
      Z <-c(DNAString(Z),DNAString('-')) #prida se mezera
      W <- c(DNAString(W),DNAString(Y[i]))#prida se znak z Y
    }
    Align <- c(DNAStringSet(Z),DNAStringSet(W))
    print(Align)
  }
  else if (length(Y)==0 && length(X)!=0) #delka Y je nula
  {
    for (j in 1:length(X) )  #podle poctu znaku v Y
    {
      Z <-c(DNAString(Z),DNAString(X[j])) #prida se mezera
      W <- c(DNAString(W),DNAString('-')) #prida se znak z Y
    }
    Align <- c(DNAStringSet(Z),DNAStringSet(W))
    print(Align)
  }
  else if (length(X)==1 && length(Y)==1)#delka X a Y je jedna
  {
    Z <- c(DNAString(Z),DNAString(X))
    W <- c(DNAString(W),DNAString(Y))
    Align <- c(DNAStringSet(Z),DNAStringSet(W))
    print(Align)
  }
  else
  {
    xlen <-length(X) #delka X
    xmid <- ceiling(xlen/2)#pulka delky X
    ylen <- length(Y)#delka Y

    ScoreL <-NWScore(X[1:xmid],Y,match,mismatch,gap) # NW skore pro prvni polovinu X a cele Y
    ScoreR <-NWScore(X[xlen:(xmid+1)],Y[ylen:1],match,mismatch,gap)# NW skore pro druhou polovinu X a cele Y (obe sek jsou obracene)
    soucet<-ScoreL+ScoreR[length(ScoreR):1]
    pozice_maxima<-which.max(soucet)
    ymid <- pozice_maxima-1#index deleni Y

    #pro prvni cast
    if (ymid==0)#index deleni Y je nula
    {
      Align <- Hirschberg(X[1:xmid],DNAString(),Align,match,mismatch,gap)#volani Hirschberg pro prvni polovinu X a prazdny vektor Y
    }
    else
    {
      Align <- Hirschberg(X[1:xmid],Y[1:ymid],Align,match,mismatch,gap)#volani Hirschberg pro prvni polovinu X a prvni cast Y
    }

    #pro druhou cast
    if ((xmid+1)>xlen) #X jiz nelze pulit
    {
      Align <- Hirschberg(DNAString(),Y[(ymid+1):ylen],Align,match,mismatch,gap)#volani Hirschberg pro prazdny vektor X a druhou cast Y
    }
    else if ((ymid+1)>ylen) #Y jiz nejde delit
    {
      Align <- Hirschberg(X[(xmid+1):xlen],DNAString(),Align,match,mismatch,gap)#volani Hirschberg pro druhou polovinu X a prazdny vektor Y
    }
    else
    {
      Align <- Hirschberg(X[(xmid+1):xlen],Y[(ymid+1):ylen],Align,match,mismatch,gap)#volani Hirschberg pro druhou polovinu X a druhou cast Y
    }
  }
  return(Align)
}
