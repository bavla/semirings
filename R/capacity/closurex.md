# Capacity semiring / Extended closure

October 3, 2025

To be able to reconstruct also the minimal paths we extend the pairs (d,w) to triples (d,w,t) - the pair (d,w) is realized by passing through the node t.

## Extended operations
```
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
```
## Extended closure
```
> L <- CJ(nodes$name,nodes$name)
> ZZ <- vector("list",n*n)
> for(i in 1:n**2) ZZ[[i]] <- cbind(Z,0)
> C <- data.frame(from=L$V1,to=L$V2)
> C$cw <- ZZ
> for(p in 1:m){
+   uv <- as.vector(ends(N,p,names=FALSE))
+   i <- (uv[1]-1)*n + uv[2]
+   C$cw[i] <- list(cbind(rbind(E(N)$cw[p][[1]]),0))
+ }
> # CC <- C
> # C <- CC
> for(t in 1:n){
+   for(u in 1:n) {
+     for(v in 1:n) { uv <- (u-1)*n + v
+       ut <- (u-1)*n + t; tv <- (t-1)*n + v
+       C$cw[uv][[1]] <- sumT(CW(uv),mulT(CW(ut),CW(tv),t))
+     }
+   }
+   tt <- (t-1)*n + t; C$cw[tt][[1]] <- sumT(cbind(E,0),CW(tt))
+ }
```

## Example

```
> C
   from to                                                     cw
1     a  a                                              0, Inf, 0
2     a  b                                              4, Inf, 0
3     a  c                         3, 6, 16, 20, 30, Inf, 0, 2, 4
4     a  d                                             10, Inf, 0
5     a  e                       10, 13, 23, 20, 30, Inf, 3, 3, 4
6     a  f 10, 11, 14, 24, 31, 15, 20, 30, 40, Inf, 7, 3, 3, 4, 7
7     a  g             7, 15, 18, 28, 15, 20, 30, Inf, 2, 3, 3, 4
8     b  a                                  5, Inf, 20, Inf, 3, 7
9     b  b                                              0, Inf, 0
10    b  c                                  2, Inf, 30, Inf, 0, 7
11    b  d                                 14, Inf, 25, Inf, 5, 7
12    b  e                                  9, Inf, 30, Inf, 3, 7
13    b  f                       6, 10, Inf, 15, 30, Inf, 7, 3, 7
14    b  g                       3, 14, Inf, 15, 30, Inf, 0, 3, 7
15    c  a                                  3, Inf, 20, Inf, 0, 7
16    c  b                                  7, Inf, 20, Inf, 1, 7
17    c  c                                              0, Inf, 0
18    c  d                                 12, Inf, 25, Inf, 5, 7
19    c  e                                              7, Inf, 0
20    c  f                                   8, 15, 40, Inf, 0, 7
21    c  g                                  10, 12, 15, Inf, 2, 0
22    d  a                                  9, Inf, 20, Inf, 3, 7
23    d  b                                 13, Inf, 20, Inf, 3, 7
24    d  c                                              6, Inf, 0
25    d  d                                              0, Inf, 0
26    d  e                                   5, 13, 25, Inf, 0, 3
27    d  f                                  14, 21, 40, Inf, 5, 7
28    d  g                                  16, 18, 15, Inf, 3, 3
29    e  a                                 14, Inf, 20, Inf, 4, 7
30    e  b                                 18, Inf, 20, Inf, 4, 7
31    e  c                                 11, Inf, 25, Inf, 4, 7
32    e  d                                  5, Inf, 25, Inf, 0, 7
33    e  e                                              0, Inf, 0
34    e  f                                              9, Inf, 0
35    e  g                      21, 23, Inf, 15, 25, Inf, 4, 4, 7
36    f  a                                 23, Inf, 20, Inf, 5, 7
37    f  b                                 27, Inf, 20, Inf, 5, 7
38    f  c                                 20, Inf, 25, Inf, 5, 7
39    f  d                                 14, Inf, 25, Inf, 5, 7
40    f  e                                              9, Inf, 0
41    f  f                                              0, Inf, 0
42    f  g                      30, 32, Inf, 15, 25, Inf, 5, 5, 7
43    g  a                                 26, Inf, 20, Inf, 6, 7
44    g  b                                 30, Inf, 20, Inf, 6, 7
45    g  c                                 23, Inf, 25, Inf, 6, 7
46    g  d                                 17, Inf, 25, Inf, 6, 7
47    g  e                                             12, Inf, 6
48    g  f                                              3, Inf, 0
49    g  g                                              0, Inf, 0
> CW(3)
     [,1] [,2] [,3]
[1,]    3   20    0
[2,]    6   30    2
[3,]   16  Inf    4
> CW(6)
     [,1] [,2] [,3]
[1,]   10   15    7
[2,]   11   20    3
[3,]   14   30    3
[4,]   24   40    4
[5,]   31  Inf    7
> cbind(nodes$name)
     [,1]
[1,] "a" 
[2,] "b" 
[3,] "c" 
[4,] "d" 
[5,] "e" 
[6,] "f" 
[7,] "g"  
```

## Saving closure to file
```
> CNx <- graph_from_data_frame(C,directed=TRUE,vertices=nodes)
> CNx$name <- "semiT2 extended closure"
> CNx$tit <- "Extended closure of the Test network 2 for the capacity semiring"
> CNx$by <- "Vladimir Batagelj"
> CNx$cdate <- date()
> saveRDS(CNx,file="xclosureT2.rds")
> write_graph_netsJSON(CNx,file="xclosureT2.json")
```


## Path reconstruction

https://github.com/bavla/semirings/blob/master/shortest.R

```
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
```

```
> setwd("C:/Users/vlado/work/R/semi")
> source("capacity.R")
> source("https://raw.githubusercontent.com/bavla/Nets/refs/heads/master/netsWeight/netsWeight.R")
> library(jsonlite)
> library(igraph)
> library(data.table)
> CW <- function(p) rbind(C$cw[p][[1]])
> CNx <- readRDS("xclosureT2.rds")
> CNx
IGRAPH 273a732 DN-- 7 49 -- semiT2 extended closure
+ attr: name (g/c), tit (g/c), by (g/c), cdate (g/c), name (v/c), x (v/n), y
| (v/n), cw (e/x)
+ edges from 273a732 (vertex names):
 [1] a->a a->b a->c a->d a->e a->f a->g b->a b->b b->c b->d b->e b->f b->g c->a c->b c->c
[18] c->d c->e c->f c->g d->a d->b d->c d->d d->e d->f d->g e->a e->b e->c e->d e->e e->f
[35] e->g f->a f->b f->c f->d f->e f->f f->g g->a g->b g->c g->d g->e g->f g->g
> n <- gorder(CNx); m <- gsize(CNx)
> nodes <- as_data_frame(CNx,what="vertices")
> links <- as_data_frame(CNx,what="edges")
> Z <- rbind(c(Inf,Inf)); E <- rbind(c(0,Inf))
> C <- links
> CW(6)
     [,1] [,2] [,3]
[1,]   10   15    7
[2,]   11   20    3
[3,]   14   30    3
[4,]   24   40    4
[5,]   31  Inf    7
> u <- 1; v <- 6; c <- 35
> nodes$name[path(C,u,v,35)]
[1] "a" "d" "c" "e" "f"
> (P <- paths(C,u,v))
   d   c             P
1 10  15    1, 2, 7, 6
2 11  20       1, 3, 6
3 14  30    1, 2, 3, 6
4 24  40 1, 4, 3, 5, 6
5 31 Inf 1, 4, 3, 7, 6
> Q <- P
> for(i in 1:nrow(P)) Q$P[i] <- list(nodes$name[P$P[i][[1]]])
> Q
   d   c             P
1 10  15    a, b, g, f
2 11  20       a, c, f
3 14  30    a, b, c, f
4 24  40 a, d, c, e, f
5 31 Inf a, d, c, g, f
> 

```
<img src="https://github.com/bavla/semirings/blob/master/R/capacity/semiT2.png" width="400" />

```

```



<hr />

[Index](README.md)
