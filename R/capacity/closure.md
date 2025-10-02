# Capacity semiring / Closure

October 2, 2025

Monika Cerinšek, Vladimir Batagelj: Semirings and Matrix Analysis of Networks

```
> ends(N,3,names=TRUE)
     [,1] [,2]
[1,] "a"  "d" 
> ends(N,3,names=FALSE)
     [,1] [,2]
[1,]    1    4
```

```
> setwd("C:/Users/vlado/work/R/semi")
> source("capacity.R")
> library(igraph)
> library(data.table)
> CW <- function(p) rbind(C$cw[p][[1]])
> N <- readRDS("semiT2.rds")
> n <- gorder(N); m <- gsize(N)
> nodes <- as_data_frame(N,what="vertices")
> links <- as_data_frame(N,what="edges")
> Z <- rbind(c(Inf,Inf)); E <- rbind(c(0,Inf))
> # initialize closure matrix
> L <- CJ(nodes$name,nodes$name)
> ZZ <- vector("list",n*n)
> for(i in 1:n**2) ZZ[[i]] <- Z
> C <- data.frame(from=L$V1,to=L$V2)
> C$cw <- ZZ
> for(p in 1:m){
+   uv <- as.vector(ends(N,p,names=FALSE))
+   i <- (uv[1]-1)*n + uv[2]
+   C$cw[i] <- E(N)$cw[p]
+ }
> # Fletcher algorithm
> for(t in 1:n){
+   for(u in 1:n) {
+     for(v in 1:n) { uv <- (u-1)*n + v
+       ut <- (u-1)*n + t; tv <- (t-1)*n + v
+       C$cw[uv][[1]] <- sumW(CW(uv),mulW(CW(ut),CW(tv)))
+     }
+   }
+   tt <- (t-1)*n + t; C$cw[tt][[1]] <- sumW(E,CW(tt))
+ }
```



```
> C
   from to                                      cw
1     a  a                                  0, Inf
2     a  b                                  4, Inf
3     a  c                   3, 6, 16, 20, 30, Inf
4     a  d                                 10, Inf
5     a  e                 10, 13, 23, 20, 30, Inf
6     a  f 10, 11, 14, 24, 31, 15, 20, 30, 40, Inf
7     a  g          7, 15, 18, 28, 15, 20, 30, Inf
8     b  a                         5, Inf, 20, Inf
9     b  b                                  0, Inf
10    b  c                         2, Inf, 30, Inf
11    b  d                        14, Inf, 25, Inf
12    b  e                         9, Inf, 30, Inf
13    b  f                 6, 10, Inf, 15, 30, Inf
14    b  g                 3, 14, Inf, 15, 30, Inf
15    c  a                         3, Inf, 20, Inf
16    c  b                         7, Inf, 20, Inf
17    c  c                                  0, Inf
18    c  d                        12, Inf, 25, Inf
19    c  e                                  7, Inf
20    c  f                          8, 15, 40, Inf
21    c  g                         10, 12, 15, Inf
22    d  a                         9, Inf, 20, Inf
23    d  b                        13, Inf, 20, Inf
24    d  c                                  6, Inf
25    d  d                                  0, Inf
26    d  e                          5, 13, 25, Inf
27    d  f                         14, 21, 40, Inf
28    d  g                         16, 18, 15, Inf
29    e  a                        14, Inf, 20, Inf
30    e  b                        18, Inf, 20, Inf
31    e  c                        11, Inf, 25, Inf
32    e  d                         5, Inf, 25, Inf
33    e  e                                  0, Inf
34    e  f                                  9, Inf
35    e  g                21, 23, Inf, 15, 25, Inf
36    f  a                        23, Inf, 20, Inf
37    f  b                        27, Inf, 20, Inf
38    f  c                        20, Inf, 25, Inf
39    f  d                        14, Inf, 25, Inf
40    f  e                                  9, Inf
41    f  f                                  0, Inf
42    f  g                30, 32, Inf, 15, 25, Inf
43    g  a                        26, Inf, 20, Inf
44    g  b                        30, Inf, 20, Inf
45    g  c                        23, Inf, 25, Inf
46    g  d                        17, Inf, 25, Inf
47    g  e                                 12, Inf
48    g  f                                  3, Inf
49    g  g                                  0, Inf
> C$cw[6]
[[1]]
     [,1] [,2]
[1,]   10   15
[2,]   11   20
[3,]   14   30
[4,]   24   40
[5,]   31  Inf
> C$cw[46]
[[1]]
     [,1] [,2]
[1,]   17   25
[2,]  Inf  Inf
```


```
> CN <- graph_from_data_frame(C,directed=TRUE,vertices=nodes)
> CN$name <- "semiT2 closure"
> CN$tit <- "Closure of the Test network 2 for the capacity semiring"
> CN$by <- "Vladimir Batagelj"
> CN$cdate <- date()
> saveRDS(CN,file="closureT2.rds")
> write_graph_netsJSON(CN,file="closureT2.json")
```


<hr />

[Index](README.md)
