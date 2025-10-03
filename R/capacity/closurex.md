# Capacity semiring / Closure

October 3, 2025


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
> 
```
<img src="https://github.com/bavla/semirings/blob/master/R/capacity/semiT2.png" width="400" />


```
> CNx <- graph_from_data_frame(C,directed=TRUE,vertices=nodes)
> CNx$name <- "semiT2 extended closure"
> CNx$tit <- "Extended closure of the Test network 2 for the capacity semiring"
> CNx$by <- "Vladimir Batagelj"
> CNx$cdate <- date()
> saveRDS(CNx,file="xclosureT2.rds")
> write_graph_netsJSON(CNx,file="xclosureT2.json")
```


<hr />

[Index](README.md)
