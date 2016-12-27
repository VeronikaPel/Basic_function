#' Function NWScore return the last row of scoring matrix creating by  Needleman-Wunsch scoring system.
#' @details sek1 and sek2 are the sekvence of DNA, which we wonted align
#' @export

NWScore <- function(Sek1,Sek2,match,mismatch,gap){
  m <- 1+length(Sek1)
  n <- 1+length(Sek2)
  S <- (0:(n-1))*gap
  #print(S)
  for (i in 2:m){
    s <- S[1]
    c <- S[1]+gap
    S[1] <- c
    for (j in 2:n){
      if (Sek1[i-1]==Sek2[j-1]){pom <- match} else {pom <- mismatch}
      c <- max(c(S[j]+gap,c+gap,s+pom))
      s <- S[j]
      S[j] <- c
    }
    #print(S)
  }
  return(S)
}
