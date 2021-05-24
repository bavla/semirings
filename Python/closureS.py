# Computing a matrix C of all paths in a given graph G
# V.B.  13.5.2012 / 14.7.2017 / 21.7.2017
from copy import deepcopy 
G = [ [ 0, 1, 1, 0],
      [ 0, 0, 1, 0],
      [ 0, 1, 0, 1],
      [ 1, 0, 0, 0] ]

n = len(G)
E = set([(v+1,) for v in range(n)])

def transit(G):
  R = deepcopy(G); n = len(G)
  for u in range(n):
    for v in range(n):
      C = set()
      if G[u][v] != 0: C.add((u+1,v+1))
      R[u][v] = C
  return R

def times(A,B):
  C = set()
  if (not A)|(not B): return C
  for a in A:
    if (a[0]==a[-1]) and (len(a)>1): continue
    for b in B:
      if (a[-1] == b[0]):
        if (b[0]==b[-1]) and (len(b)>1): continue
        if a[0] == b[-1]:
          if set(a[1:]).isdisjoint(set(b[1:])): C.add(a+b[1:])
        else:  
          if set(a).isdisjoint(set(b[1:])): C.add(a+b[1:])
  return C

def closure(R): # Fletcher's algorithm / strict closure
  n = len(R); C = deepcopy(R)
  for k in range(n):
    for u in range(n):
      for v in range(n): 
        C[u][v] = C[u][v] | times(C[u][k],C[k][v])
  return C

def output(R):
  n = len(R)
  for u in range(n):
    for v in range(n): print(u+1,v+1,R[u][v])

def outvec(D):
  for u in range(len(D)): print(u+1,D[u])

def hamilton(D):
  n = len(D); H = [0]*n
  for u in range(n):
    S = set()
    for p in D[u][u]:
      if len(p)>n: S.add(p)
    H[u] = S
  return H

def step(R,V):
  n = len(R); U = [set()]*n
  for u in range(n):
    S = set()
    for v in range(n):
      S = S | times(V[v],R[v][u])
    U[u] = S
  return U
    
print('matrix G'); output(G)

R = transit(G)
print('\ntransition matrix R'); output(R)

C = closure(R)
print('\nclosure matrix C'); output(C)

H = hamilton(C)
print('\nhamilton cycles vector H'); outvec(H)

V = [ set() for i in range(n)]; V[2] = set([(3,)])

V1 = step(R,V);  print("V1"); outvec(V1)
V2 = step(R,V1); print("V2"); outvec(V2)
V3 = step(R,V2); print("V3"); outvec(V3)
V4 = step(R,V3); print("V4"); outvec(V4)

