# betweenness.R
# computing betweenness using geodesic semiring
# Batagelj, V: Semirings for social networks analysis. J Math Sociol 19 (1): 53-68 1994
# by Vladimir Batagelj
# ver 1. December 2003
#

mat.geodesics <- function(m)
{ n <- nrow(m)
  md <- m; md[m==0] <- Inf
  mc <- m; mc[m>0] <- 1
  for(k in 1:n) { for(u in 1:n){ for(v in 1:n){
    dst <- md[u,k] + md[k,v]
    if(md[u,v] >= dst) {
      cnt <- mc[u,k]*mc[k,v];
      if (md[u,v] == dst) {mc[u,v] <- mc[u,v] + cnt }
      else{ md[u,v] <- dst; mc[u,v] <- cnt }
    }
  }}}
  return(list(dis=md,cnt=mc))
}

vec.Closeness <- function(dis)
{ n <- nrow(dis); return((n-1)/rowSums(dis)) }

vec.Betweenness <- function(dis,cnt){
  n <- nrow(dis); bw <- rep(0,n)
  for (v in 1:n) {
    b <- 0
    for(u in 1:n) { for(w in 1:n) {
      if((cnt[u,w] > 0) && (u != w) && (u != v) && (v != w) &&
            ((dis[u,v] + dis[v,w]) == dis[u,w]))
        {b <- b + cnt[u,v]*cnt[v,w] / cnt[u,w]}
    }}
    bw[v] <- b/((n-1)*(n-2))
  }
  return(bw)
}

vec.betweenness <- function(m)
{ mt <- mat.geodesics(m); return(vec.Betweenness(mt$dis,m$tcnt)) }

g <- c(
 0, 1, 1, 0, 0, 0, 0, 0,
 0, 0, 0, 1, 1, 0, 0, 0,
 0, 0, 0, 1, 1, 0, 0, 0,
 0, 0, 0, 0, 0, 1, 1, 0,
 0, 0, 0, 0, 1, 1, 1, 0,
 0, 0, 0, 0, 0, 0, 0, 1,
 0, 0, 0, 0, 0, 0, 0, 1,
 0, 1, 0, 0, 0, 0, 0, 0  )
n <- 1:8
g <- matrix(nrow=8,byrow=T,dimnames=list(n,n),data=g)

# G <- mat.geodesics(g)
# c <- vec.Closeness(G$dis)
# b <- vec.Betweenness(G$dis,G$cnt)
# bb <- vec.betweenness(g)
