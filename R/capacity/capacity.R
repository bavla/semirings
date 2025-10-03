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

closureW <- function(N){
  n <- gorder(N); nodes <- as_data_frame(N,what="vertices")
  Z <- rbind(c(Inf,Inf)); E <- rbind(c(0,Inf))
# initialize closure matrix
  L <- CJ(nodes$name,nodes$name)
  ZZ <- vector("list",n*n)
  for(i in 1:n**2) ZZ[[i]] <- Z
  C <- data.frame(from=L$V1,to=L$V2); C$cw <- ZZ
  for(p in 1:m){ uv <- as.vector(ends(N,p,names=FALSE))
    C$cw[(uv[1]-1)*n + uv[2]] <- E(N)$cw[p]
  }
# Fletcher algorithm
  for(t in 1:n){
    for(u in 1:n) for(v in 1:n) { uv <- (u-1)*n + v
      ut <- (u-1)*n + t; tv <- (t-1)*n + v
      C$cw[uv][[1]] <- sumW(CW(uv),mulW(CW(ut),CW(tv)))
    }
    tt <- (t-1)*n + t; C$cw[tt][[1]] <- sumW(E,CW(tt))
  }
  return(C)
}

# N <- readRDS("semiT2.rds")
# ClN <- closureW(N)

# Extended capacity semiring operations
# October 3, 2025

sumT <- function(A,B){
  na = nrow(A); nb <- nrow(B)
  ia <- 1; ib <- 1; C <- NULL; do <- -1
  repeat {
    if(ia<=na) {da <- A[ia,1]; ta <- A[ia,2]; ka <- A[ia,3]} else {da <- ta <- Inf}
    if(ib<=nb) {db <- B[ib,1]; tb <- B[ib,2]; kb <- B[ib,3]} else {db <- tb <- Inf}
    if(da<db) {d <- da; k <- ka} else {d <- db; k <- kb} 
    t <- min(ta,tb)
    if(d==do) C[nrow(C),2] <- t else C <- rbind(C,c(d,t,k))
    do <- d
    if(t>=ta) ia <- ia+1
    if(t>=tb) ib <- ib+1
    if((ia>na)&(ib>nb)) break
  }
  return(C)
}

mulT <- function(A,B,k){
  na = nrow(A); nb <- nrow(B)
  ia <- 1; ib <- 1; C <- NULL; do <- -1
  while((ia<=na)&(ib<=nb)) {
    ta <- A[ia,2]; tb <- B[ib,2]
    d <- A[ia,1]+B[ib,1]; t <- min(ta,tb)
    if(d==do) C[nrow(C),2] <- t else C <- rbind(C,c(d,t,k))
    do <- d
    if(t>=ta) ia <- ia+1
    if(t>=tb) ib <- ib+1
  }
  return(C)
}

# Extended closure

closureT <- function(N){
  n <- gorder(N); nodes <- as_data_frame(N,what="vertices")
  Z <- rbind(c(Inf,Inf)); E <- rbind(c(0,Inf))
# initialize closure matrix
  L <- CJ(nodes$name,nodes$name)
  ZZ <- vector("list",n*n)
  for(i in 1:n**2) ZZ[[i]] <- cbind(Z,0)
  C <- data.frame(from=L$V1,to=L$V2); C$cw <- ZZ
  for(p in 1:m){ uv <- as.vector(ends(N,p,names=FALSE)) 
    C$cw[(uv[1]-1)*n + uv[2]] <- list(cbind(rbind(E(N)$cw[p][[1]]),0))
  }
# Fletcher algorithm
  for(t in 1:n){
    for(u in 1:n) for(v in 1:n) { uv <- (u-1)*n + v
      ut <- (u-1)*n + t; tv <- (t-1)*n + v
      C$cw[uv][[1]] <- sumT(CW(uv),mulT(CW(ut),CW(tv),t))
    }
    tt <- (t-1)*n + t; C$cw[tt][[1]] <- sumT(cbind(E,0),CW(tt))
  }
  return(C)
}

# N <- readRDS("semiT2.rds")
# CxN <- closureT(N)

# paths construction
# https://github.com/bavla/semirings/blob/master/shortest.R

nodex <- function(C,u,v,c){
  uv <- (u-1)*n + v; T <- CW(uv)
  for(i in 1:nrow(T)) if(T[i,2]>=c) break
  return(T[i,3])
}

.path <- function(C,u,v,c){
  k <- nodex(C,u,v,c)
  if(k == 0) return(NULL)
  return(c(.path(C,u,k,c),k,.path(C,k,v,c)))
}

path <- function(C,u,v,c){
  if(nodex(C,u,v,c) == Inf) return(NULL)
  return(c(u,.path(C,u,v,c),v))
}

paths <- function(C,u,v){
  uv <- (u-1)*n + v; T <- CW(uv)
  P <- data.frame(d=T[,1],c=T[,2]); P$P <- vector("list",nrow(T))
  for(i in 1:nrow(T)) P$P[i] <- list(path(C,u,v,T[i,2]))
  return(P)
} 
