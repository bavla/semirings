# Length and capacity emiring
# VB, Sep-Oct, 2025
# https://github.com/bavla/semirings/tree/master/R/capacity
# source("https://raw.githubusercontent.com/bavla/semirings/refs/heads/master/R/capacity/capacity.R")


> setwd("C:/Users/vlado/work/R/semi")
> library(igraph); library(data.table); library(jsonlite)
> source("https://raw.githubusercontent.com/bavla/semirings/refs/heads/master/R/capacity/capacity.R")
> # source("capacity.R")
> source("https://raw.githubusercontent.com/bavla/Nets/refs/heads/master/netsWeight/netsWeight.R")
> # source("netsWeight.R")

> # Length and capacity emiring
> A <- rbind( c(7,15), c(15,20), c(18,30), c(28,Inf) )
> B <- rbind( c(11,20), c(14,30), c(24,40), c(31,Inf) )
> C <- rbind( c(10,20), c(15,Inf) )
> D <- rbind( c(7,15), c(20,Inf) )
> Z <- rbind( c(Inf,Inf) )
> E <- rbind( c(0,Inf) )

> sumW(A,Z)
> mulW(A,E)
> sumW(A,E)
> mulW(A,Z)

> S <- sumW(A,B)
> R <- mulW(A,B)
> xMax <- 50; yMax <- 60;
> plot(0,0,xlim=c(0,xMax),ylim=c(0,yMax),type="n",main="sumW and mulW",
+   xlab="w",ylab="d")
> plotW(S,col="gray",lwd=6,cex=2)
> plotW(R,col="green",lwd=6,cex=2)
> plotW(A,col="red",lwd=1)
> plotW(B,col="blue",lwd=1)
> legend("topleft",c("A","B","sumW","mulW"), pch=c(16,16,16,16), 
+   col=c("red","blue","gray","green"))

> # Network
> N <- readRDS("semiT2.rds")
> sapply(graph_attr_names(N),function(x) graph_attr(N,x))
> n <- gorder(N); m <- gsize(N)
> nodes <- as_data_frame(N,what="vertices")
> links <- as_data_frame(N,what="edges")
> CX <- closureT(N)

> CNx <- graph_from_data_frame(CX,directed=TRUE,vertices=nodes)
> CNx$name <- "semiT2 extended closure"
> CNx$tit <- "Extended closure of the Test network 2 for the capacity semiring"
> CNx$by <- "Vladimir Batagelj"
> CNx$cdate <- date()
> saveRDS(CNx,file="xclosureT2.rds")
> write_graph_netsJSON(CNx,file="xclosureT2.json")

> CX
> rbind(V(N)$name)
> CW(CX,6)
> (P <- paths(CX,1,6))
> Q <- P
> for(i in 1:nrow(P)) Q$P[i] <- list(nodes$name[P$P[i][[1]]])
> Q

> ex <- "https://raw.githubusercontent.com/bavla/semirings/refs/heads/master/R/capacity/semiT2.json"
> T <- netsJSON_to_graph(fromJSON(ex),directed=TRUE)
> CT <- closureT(T)

