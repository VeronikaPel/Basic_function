#' PartialDigestProblem function given the (multi)set ∆X, compute a set X which could have produced ∆X in restriction mapping.
#' @description L is vector of ∆X (fragments vector)
#' @return vector of restriction place
#' @export

PartialDigestProblem <- function(L){
  width <- max(L)
  L <- L[-match(width,L)]
  X <- c(0,width)
  Place(L,X,width)
}
