# Capacity semiring
# VB, Sep 21-24, 2025
# https://github.com/bavla/semirings/tree/master/R/capacity


plotW <- function(A,col="black",lwd=2,pch=16,cex=1){
  na <- nrow(A)
  Ax <- rbind(c(0,0),A)
  Ax[na+1,2] <- min(Ax[na+1,2],50)
  for(i in 1:na) 
    points(c(Ax[i,2],Ax[i+1,2]),rep(Ax[i+1,1],2),col=col,type="l",lwd=lwd)
  points(A[,2],A[,1],col=col,pch=pch,cex=cex)
}

sumW <- function(A,B){
  na = nrow(A); nb <- nrow(B)
  ia <- 1; ib <- 1; C <- NULL; do <- -1
  repeat {
    if(ia<=na) {da <- A[ia,1]; ta <- A[ia,2]} else {da <- ta <- Inf}
    if(ib<=nb) {db <- B[ib,1]; tb <- B[ib,2]} else {db <- tb <- Inf}
    d <- min(da,db); t <- min(ta,tb)
    if(d==do) C[nrow(C),2] <- t else C <- rbind(C,c(d,t))
    do <- d
    if(t>=ta) ia <- ia+1
    if(t>=tb) ib <- ib+1
    if((ia>na)&(ib>nb)) break
  }
  return(C)
}

mulW <- function(A,B){
  na = nrow(A); nb <- nrow(B)
  ia <- 1; ib <- 1; C <- NULL; do <- -1
  while((ia<=na)&(ib<=nb)) {
    ta <- A[ia,2]; tb <- B[ib,2]
    d <- A[ia,1]+B[ib,1]; t <- min(ta,tb)
    if(d==do) C[nrow(C),2] <- t else C <- rbind(C,c(d,t))
    do <- d
    if(t>=ta) ia <- ia+1
    if(t>=tb) ib <- ib+1
  }
  return(C)
}

