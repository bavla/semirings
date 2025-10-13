# Path reconstruction

October 3, 2025

To be able to reconstruct also the minimal paths we extend the pairs (d,w) to triples (d,w,t) - the pair (d,w) is realized by passing through the node t.

## Functions

https://github.com/bavla/semirings/blob/master/shortest.R

```
CW <- function(C,p) rbind(C$cw[p][[1]])

nodex <- function(C,u,v,c){
  uv <- (u-1)*n + v; T <- CW(C,uv)
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
  uv <- (u-1)*n + v; T <- CW(C,uv)
  P <- data.frame(d=T[,1],c=T[,2]); P$P <- vector("list",nrow(T))
  for(i in 1:nrow(T)) P$P[i] <- list(path(C,u,v,T[i,2]))
  return(P)
}
```

## Computing an extended closure

```
> setwd("C:/Users/vlado/work/R/semi")
> source("capacity.R")
> source("https://raw.githubusercontent.com/bavla/Nets/refs/heads/master/netsWeight/netsWeight.R")
> library(jsonlite); library(igraph); library(data.table)
> N <- readRDS("semiT2.rds")
> CX <- closureT(N)
```

## Reading a saved extended closure

```
> setwd("C:/Users/vlado/work/R/semi")
> source("capacity.R")
> source("https://raw.githubusercontent.com/bavla/Nets/refs/heads/master/netsWeight/netsWeight.R")
> library(jsonlite)
> library(igraph)
> library(data.table)
> CX <- readRDS("xclosureT2.rds")
> CX
IGRAPH 273a732 DN-- 7 49 -- semiT2 extended closure
+ attr: name (g/c), tit (g/c), by (g/c), cdate (g/c), name (v/c), x (v/n), y
| (v/n), cw (e/x)
+ edges from 273a732 (vertex names):
 [1] a->a a->b a->c a->d a->e a->f a->g b->a b->b b->c b->d b->e b->f b->g c->a c->b c->c
[18] c->d c->e c->f c->g d->a d->b d->c d->d d->e d->f d->g e->a e->b e->c e->d e->e e->f
[35] e->g f->a f->b f->c f->d f->e f->f f->g g->a g->b g->c g->d g->e g->f g->g
> n <- gorder(CX); m <- gsize(CX)
> nodes <- as_data_frame(CX,what="vertices")
> links <- as_data_frame(CX,what="edges")
> C <- links
> CW(C,6)
     [,1] [,2] [,3]
[1,]   10   15    7
[2,]   11   20    3
[3,]   14   30    3
[4,]   24   40    4
[5,]   31  Inf    7
> u <- 1; v <- 6; c <- 35
> nodes$name[path(C,u,v,35)]
[1] "a" "d" "c" "f"
> (P <- paths(C,u,v))
   d   c             P
1 10  15    1, 2, 7, 6
2 11  20       1, 3, 6
3 14  30    1, 2, 3, 6
4 24  40    1, 4, 3, 6
5 31 Inf 1, 4, 3, 7, 6
> Q <- P
> for(i in 1:nrow(P)) Q$P[i] <- list(nodes$name[P$P[i][[1]]])
> Q
   d   c             P
1 10  15    a, b, g, f
2 11  20       a, c, f
3 14  30    a, b, c, f
4 24  40    a, d, c, f
5 31 Inf a, d, c, g, f
```
<img src="https://github.com/bavla/semirings/blob/master/R/capacity/semiT2.png" width="400" />

```

```



<hr />

[Index](README.md)
