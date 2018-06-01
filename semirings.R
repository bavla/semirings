# Semirings.R
# basic support for matrix approach to network analysis based on
# semirings.
# by Vladimir Batagelj
#  ver 0. Dec 2003
#  ver 1. May 29-31, 2018 (HSE, Moscow)
# TO DO:
# - names of nodes
# - tests of input parameters types and compatibilty
#

Sr.set <- function(sr="combinatorial",r=2){
  if(sr=="combinatorial"){
    Sr.zero <<- 0; Sr.one <<- 1; Sr.absorption <<- FALSE
    '%(+)%' <<- function(a,b) return(a+b)
    '%(.)%' <<- function(a,b) return(a*b)
    Sr.star <<- function(a) if(a==1) return(Inf) else
      if(a==Inf) return(Inf) else return(1/(1-a))
  } else if(sr=="shortpaths"){
    Sr.zero <<- Inf; Sr.one <<- 0; Sr.absorption <<- TRUE
    '%(+)%' <<- min
    '%(.)%' <<- function(a,b) return(a+b)
    Sr.star <<- function(a) return(0)
  } else if(sr=="logical"){
    Sr.zero <<- 0; Sr.one <<- 1; Sr.absorption <<- TRUE
    '%(+)%' <<- max
    '%(.)%' <<- min
    Sr.star <<- function(a) return(1)
  } else if(sr=="maxmin"){
    Sr.zero <<- 0; Sr.one <<- Inf; Sr.absorption <<- TRUE
    '%(+)%' <<- max
    '%(.)%' <<- min
    Sr.star <<- function(a) return(Inf)
  } else if(sr=="log"){
    Sr.zero <<- Inf; Sr.one <<- 0; Sr.absorption <<- FALSE
    '%(+)%' <<- function(a,b) return(-log(exp(-a)+exp(-b)))
    '%(.)%' <<- function(a,b) return(a+b)
  } else if(sr=="pathfinder"){
    Sr.zero <<- Inf; Sr.one <<- 0; Sr.absorption <<- TRUE
    '%(+)%' <<- min
    '%(.)%' <<- function(a,b){return((a^r+b^r)^(1/r))}
    Sr.r <<- r
    Sr.star <<- function(a) return(0)
  } else {cat("unknown semiring\n"); return(NULL)}
  Sr.type <<- sr
}

Sr.set()

Sr.Zero <- function(n) return(matrix(Sr.zero,nrow=n,ncol=n))

Sr.One <- function(n){ I <- Sr.Zero(n); diag(I) <- Sr.one; return(I)}

Sr.adapt <- function(A) {P <- A; P[P==0] <- Sr.zero; return(P)}

Sr.binary <- function(A) {B <- A; B[B!=Sr.zero] <- Sr.one; return(B)}

Sr.select <- function(n,s) {S <- rep(Sr.zero,n); S[s] <- Sr.one; return(S)}

Sr.Perm <- function(A,p) return(A[p,p])

Sr.perminv <- function(p) {q <- p; q[p] <- 1:length(p); return(q)}

'%[+]%' <- Sr.Plus <- function(A,B){
  if(is.null(dim(A))) {na <- length(A); C <- numeric(na)
    for(i in 1:na) C[i] <- A[i] %(+)% B[i]; return(C)}
  na <- nrow(A); ma <- ncol(A)
  C <- matrix(0,nrow=na,ncol=ma)
  for(i in 1:na) for(j in 1:ma) C[i,j] <- A[i,j] %(+)% B[i,j]
  return(C)
}

'%[.]%' <- Sr.Times <- function(A,B){ vector <- FALSE
  if(is.null(dim(A))) {A <- matrix(A,ncol=length(A),nrow=1); vector <- TRUE}
  if(is.null(dim(B))) {B <- matrix(B,nrow=length(B),ncol=1); vector <- TRUE}
  na <- nrow(A); ma <- ncol(A); nb <- nrow(B); mb <- ncol(B)
  if(ma!=nb) {cat("incompatible matrices\n"); return(NULL)}
  C <- matrix(Sr.zero,nrow=na,ncol=mb)
  for(i in 1:na) for(j in 1:mb) { s <- Sr.zero
    for(k in 1:ma) s <- s %(+)% (A[i,k] %(.)% B[k,j])
    C[i,j] <- s
  }
  if(vector) return(as.vector(C)) else return(C)
}

Sr.Power <- function(A,k){
  n <- nrow(A); Mt <- Sr.One(n)
  rownames(Mt) <- colnames(Mt) <- rownames(A)
  if (k > 0) { i <- k; Ms <- A
    repeat {
      if ((i %% 2) == 1) { Mt <- Mt %[.]% Ms }
      i <- i %/% 2; if (i == 0) break
      Ms <- Ms %[.]% Ms
    }
  }
  return(Mt)
}

.spw <- function(i,k,M){
  n <- nrow(M)
  if (i == 1) return(list(A=M %[+]% Sr.One(n), B=M %[.]% M))
  if (i %% 2 == 0) {
    AB <- .spw(i-1,k,M)
    list(A=AB$A %[+]% AB$B, B=if(i<k){AB$B %[.]% M}else{NULL})
  } else {
    AB <- .spw(i %/% 2,k,M)
    P <- AB$B %[+]% Sr.One(n)
    list(A=P %[.]% AB$A, B=if(i<k){AB$B %[.]% AB$B}else{NULL})
  }
}

Sr.Sumpow <- function(A,k){ n <- nrow(A)
  if (k == 0) return(Sr.One(n))
  if (k == 1) return(Sr.One(n) %[+]% A)
  return(.spw(k,k,A)$A)
}

Sr.Closure <- function(A){
  if(!Sr.absorption) {cat("Semiring is not absorptive\n"); return(NULL)}
  na <- nrow(A); C <- A
  for(k in 1:na) {for(i in 1:na) for(j in 1:na)
    C[i,j] <- C[i,j] %(+)% (C[i,k] %(.)% Sr.star(C[k,k]) %(.)% C[k,j])
    C[k,k] <- Sr.one %(+)% C[k,k]
  }
  return(C)
}

Sr.save.net <- function(fnet,A){
  net <- file(fnet,"w")
  n <- nrow(A); rn <- rownames(A)
  cat("*vertices",n,"\n",file=net)
  for (i in 1:n) cat(i," \"",rn[i],"\"\n",file=net,sep="")
  cat("*matrix\n",file=net)
  for (i in 1:n) cat(A[i,],"\n",file=net)
  close(net)
}

# Examples

w <- c(
  0, 2, 3,  5, 0, 0,  0, 0, 0,
  0, 0, 0,  2, 4, 0,  0, 0, 0,
  0, 0, 0,  4, 0, 0,  3, 0, 0,
  0, 0, 0,  0, 3, 0,  2, 0, 0,
  0, 0, 0,  0, 0, 5,  0, 2, 0,
  0, 0, 0,  1, 0, 3,  5, 0, 0,
  5, 0, 0,  0, 0, 0,  0, 0, 0,
  0, 0, 0,  0, 0, 0,  0, 0, 0,
  0, 0, 0,  0, 0, 2,  0, 0, 0  )
W <- matrix(w,byrow=TRUE,nrow=9)
colnames(W) <- rownames(W) <- paste("v",1:9,sep="")

