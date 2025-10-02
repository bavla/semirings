# Capacity semiring / Test network 1

October 1, 2025

## Creating iGraph network with capacity weights

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
> library(jsonlite)
> write_graph_netsJSON(N,file="semiT.json")
```

## Visualization and adding coordinates

Because the name weight is reserved in the igraph we rename the capacity weight to cw
```
> E(N)$cw <- E(N)$weight
> E(N)$weight <- 1
> N
> Pt <- tkplot(N,800,800,edge.curved=0,edge.width=2)
# tkplot window is still active
> coor <- tk_coords(Pt,norm=F) # save new coordinates
> tk_close(Pt)
> V(N)$x <- coor[,1]; V(N)$y <- coor[,2]
> saveRDS(N,file="semiT.rds")
> write_graph_netsJSON(N,file="semiT.json")
> links <- as_data_frame(N,what="edges")
> links
   from to weight      cw
1     a  b      1  4, Inf
2     a  c      1   3, 20
3     a  d      1 10, Inf
4     b  c      1   2, 30
5     d  c      1  6, Inf
6     b  g      1   3, 15
7     d  e      1   5, 25
8     c  g      1 12, Inf
9     c  e      1  7, Inf
10    c  f      1   8, 40
11    e  f      1  9, Inf
12    g  f      1  3, Inf
> lab <- gsub(" ","",gsub("Inf","∞",gsub("c","",as.character(E(N)$cw))))
> plot(N,vertex.size=20,vertex.label.cex=2,edge.label=lab,edge.label.cex=1.5)
```
<img src="https://github.com/bavla/semirings/blob/master/R/capacity/semiT.png" width="400" />

## Computing with capacity weights

```
> ab <- rbind(links$cw[1][[1]]); bc <- rbind(links$cw[4][[1]]); ac <- rbind(links$cw[2][[1]])
> ad <- rbind(links$cw[3][[1]]); dc <- rbind(links$cw[5][[1]]); bg <- rbind(links$cw[6][[1]])
> cg <- rbind(links$cw[8][[1]])
> ab
     [,1] [,2]
[1,]    4  Inf
> (act <- sumW(mulW(ab,bc),sumW(ac,mulW(ad,dc))))
     [,1] [,2]
[1,]    3   20
[2,]    6   30
[3,]   16  Inf
> (agt <- sumW(mulW(ab,bg),mulW(act,cg)))
     [,1] [,2]
[1,]    7   15
[2,]   15   20
[3,]   18   30
[4,]   28  Inf
```


<hr />

[Index](README.md)

