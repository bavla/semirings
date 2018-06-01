# shortest.R
# computing shortes paths using shortes path semiring
# by Vladimir Batagelj
# ver 1. December 2003
#

mat.paths <- function(W){
  n <- nrow(W); Wd <- W
  Wd[Wd==0] <- Inf
  diag(Wd) <- rep(0,n)
  Wt <- Wd; Wt[Wt < Inf ] <- 0
  for (k in 1:n) { for (i in 1:n){ for (j in 1:n){
    d <- Wd[i,k] + Wd[k,j]
    if (Wd[i,j] > d) { Wd[i,j] <- d; Wt[i,j] <- k }
  }}}
  list(dis=Wd,tra=Wt)
}

.path <- function(Wt,u,v){
  k <- Wt[u,v]
  if(k == 0) return(NULL)
  return(c(.path(Wt,u,k),k,.path(Wt,k,v)))
}

path <- function(Wt,u,v){
  if(Wt[u,v] == Inf) return(NULL)
  return(c(u,.path(Wt,u,v),v))
}

# W from semirings.R
# (M <- mat.paths(W))
# path(M$tra,9,2)
# path(M$tra,2,9)


