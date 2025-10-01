# Capacity semiring
September 21-24, 2025

An arc p in the weighted network (V,L,w) has weight described by a pair w(p) = (d(p),c(p)) where d(p) is the length of the arc p and c(p) is the capacity of the arc p.
These weights can be extended to the weight paths and set of paths and described by a sequence of pairs

**a** = [ (ad<sub>1</sub>, ac<sub>1</sub>), (ad<sub>2</sub>, ac<sub>2</sub>), ..., (ad<sub>k</sub>, ac<sub>k</sub>) ]

where ad<sub>i</sub>, ac<sub>i</sub> are nonnegative real numbers and

i < j ⇒ (ac<sub>i</sub> < ac<sub>j</sub>) ∧ (ad<sub>i</sub> < ad<sub>j</sub>)

We will call them the **capacity weights** and denote their set with **C**. We can introduce in **C** two operations

addition (**a**⊕**b**)(w) = min(a(w),b(w)) and <br />
multiplication (**a**⊙**b**)(w) = a(w)+b(w)

The structure (**C**, ⊕, ⊙, **0**, **1**), where **0** = [(∞, ∞)] and **1** = [(0, ∞)], is an absorptive semiring.

**1** ⊕ **a** = **1**

(**1**⊕**a**)(w) = min(1(w),a(w)) = min(0,a(w)) = 0 = 1(w)

## Capacity weights

```
> A <- rbind( c(7,15), c(15,20), c(18,30), c(28,Inf) )
> B <- rbind( c(11,20), c(14,30), c(24,40), c(31,Inf) )
> C <- rbind( c(10,20), c(15,Inf) )
> D <- rbind( c(7,15), c(20,Inf) )
> Z <- rbind( c(Inf,Inf) )
> E <- rbind( c(0,Inf) )
```

```
> A
     [,1] [,2]
[1,]    7   15
[2,]   15   20
[3,]   18   30
[4,]   28  Inf
> B
     [,1] [,2]
[1,]   11   20
[2,]   14   30
[3,]   24   40
[4,]   31  Inf
```

<img src="https://github.com/bavla/semirings/blob/master/R/capacity/sum%2Bmul.png" width="700" />

## Visualization, addition, and multiplication of capacity weights
```
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
```


```
> S <- sumW(A,B)
> R <- mulW(A,B)
> S
     [,1] [,2]
[1,]    7   15
[2,]   11   20
[3,]   14   30
[4,]   24   40
[5,]   28  Inf
> R
     [,1] [,2]
[1,]   18   15
[2,]   26   20
[3,]   32   30
[4,]   52   40
[5,]   59  Inf
> 
> xMax <- 50; yMax <- 60;
> plot(0,0,xlim=c(0,xMax),ylim=c(0,yMax),type="n",main="sumW and mulW",
+   xlab="w",ylab="d")
> plotW(S,col="gray",lwd=6,cex=2)
> plotW(R,col="green",lwd=6,cex=2)
> plotW(A,col="red",lwd=1)
> plotW(B,col="blue",lwd=1)
> legend("topleft",c("A","B","sumW","mulW"), pch=c(16,16,16,16), 
+   col=c("red","blue","gray","green"))
```

```
> sumW(A,Z)
     [,1] [,2]
[1,]    7   15
[2,]   15   20
[3,]   18   30
[4,]   28  Inf
> mulW(A,E)
     [,1] [,2]
[1,]    7   15
[2,]   15   20
[3,]   18   30
[4,]   28  Inf
> sumW(A,E)
     [,1] [,2]
[1,]    0  Inf
> mulW(A,Z)
     [,1] [,2]
[1,]  Inf  Inf 
```
## Creating iGraph network with capacity weights

October 1, 2025

https://github.com/bavla/netsJSON/blob/master/format/stru.md

```
> setwd("C:/Users/vlado/work/R/iGraph/format")
> C <- read.csv("semiT.csv",skip=1,head=TRUE,sep="")
> nodes <- c("a","b","c","d","e","f","g")
> N <- graph_from_data_frame(C[,c("f","t")],directed=TRUE,vertices=nodes)
> N
IGRAPH 9146607 DN-- 7 12 -- 
+ attr: name (v/c)
+ edges from 9146607 (vertex names):
 [1] a->b a->c a->d b->c d->c b->g d->e c->g c->e c->f e->f g->f
> L <- vector("list",12)
> for(i in 1:12) L[[i]] <- c(C[i,"d"],C[i,"c"])
> E(N)$weight <- L
> N$name <- "semiT"
> N$tit <- "Test network for the capacity semiring"
> N$by <- "Vladimir Batagelj"
> N$cdate <- date()
> N
IGRAPH 9146607 DNW- 7 12 -- semiT
+ attr: name (g/c), tit (g/c), by (g/c), cdate (g/c), name (v/c), weight (e/x)
+ edges from 9146607 (vertex names):
 [1] a->b a->c a->d b->c d->c b->g d->e c->g c->e c->f e->f g->f
> E(N)[[]]
> saveRDS(N,file="semiT.rds")
```

<hr />

[Index](../README.md)
