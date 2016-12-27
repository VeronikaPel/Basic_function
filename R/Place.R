Place <- function(L,X,width){
  if(!length(L)){
    print(X)
    return(X)
  }
  y <- max(L)
  dif <- abs(y-X)
  if(setequal(intersect(L,dif),dif)){
    X <- c(X,y)
    L <- L[-match(dif,L)]
    Place(L,X,width)
    X <- X[-match(y,X)]
    L <- c(L,dif)
  }

  dif <- abs((width-y)-X)
  if(setequal(intersect(L,dif),dif)){
    X <- c(X,(width-y))
    L <- L[-match(dif,L)]
    Place(L,X,width)
    X <- X[-match((width-y),X)]
    L <- c(L,dif)
  }
  return(X)
}
