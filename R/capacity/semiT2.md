# Capacity semiring / Test network 2

October 2, 2025
```
## Creating iGraph network with capacity weights

https://github.com/bavla/netsJSON/blob/master/format/stru.md


```
> setwd("C:/Users/vlado/work/R/semi")
> source("https://raw.githubusercontent.com/bavla/Nets/refs/heads/master/netsWeight/netsWeight.R")
> library(jsonlite)
> source("capacity.R")
> library(igraph)
> N <- readRDS("semiT.rds")
> N
> dir <- c("a","e","a","a","a","a","e","a","a","a","e","a")
> E(N)$dir <- dir
> (links <- as_data_frame(N,what="edges"))
   from to weight      cw dir
1     a  b      1  4, Inf   a
2     a  c      1   3, 20   e
3     a  d      1 10, Inf   a
4     b  c      1   2, 30   a
5     d  c      1  6, Inf   a
6     b  g      1   3, 15   a
7     d  e      1   5, 25   e
8     c  g      1 12, Inf   a
9     c  e      1  7, Inf   a
10    c  f      1   8, 40   a
11    e  f      1  9, Inf   e
12    g  f      1  3, Inf   a
> (D <- links[dir=="e",])
   from to weight     cw dir
2     a  c      1  3, 20   e
7     d  e      1  5, 25   e
11    e  f      1 9, Inf   e
> names(D)[1:2] <- c("to","from")
> D
   to from weight     cw dir
2   a    c      1  3, 20   e
7   d    e      1  5, 25   e
11  e    f      1 9, Inf   e
> (S <- rbind(links,D))
    from to weight      cw dir
1      a  b      1  4, Inf   a
2      a  c      1   3, 20   e
3      a  d      1 10, Inf   a
4      b  c      1   2, 30   a
5      d  c      1  6, Inf   a
6      b  g      1   3, 15   a
7      d  e      1   5, 25   e
8      c  g      1 12, Inf   a
9      c  e      1  7, Inf   a
10     c  f      1   8, 40   a
11     e  f      1  9, Inf   e
12     g  f      1  3, Inf   a
21     c  a      1   3, 20   e
71     e  d      1   5, 25   e
111    f  e      1  9, Inf   e
> rbind(S$cw[13][[1]])
     [,1] [,2]
[1,]    3   20
> graph_attr(N)

> nodes <- as_data_frame(N,what="vertices")
> N2 <- graph_from_data_frame(S,directed=TRUE,vertices=nodes)
> N2$name <- "semiT2"
> N2$tit <- "Test network 2 for the capacity semiring"
> N2$by <- "Vladimir Batagelj"
> N2$cdate <- date()
> N2$mo <- 12
> saveRDS(N2,file="semiT2.rds")
> write_graph_netsJSON(N2,file="semiT2.json")

> lab <- gsub(" ","",gsub("Inf","∞",gsub("c","",as.character(E(N2)$cw))))
> lab[(N2$mo+1):length(lab)] <- ""
> plot(N2,vertex.size=20,vertex.label.cex=2,edge.label=lab,edge.label.cex=1.5)
```

<img src="https://github.com/bavla/semirings/blob/master/R/capacity/semiT.png" width="400" />

```
```


<hr />

[Index](../README.md)
