# semirings 

https://github.com/bavla/semirings/wiki/Refs

basic support in R for network analysis using matrices over semirings

The active semiring has two binary operations %(+)% and %(.)% with neutral elements Sr.zero and Sr.one. 

> source("https://github.com/bavla/semirings/raw/master/semirings.R")

Sr.set  (sr="combinatorial",r=2)
  sets the active semiring. sr can be "combinatorial", "shortpaths", "logical", "maxmin", "log", "pathfinder"

Sr.get  () 
  returns the name of the active semiring

Sr.star (a) closure unary operation in absorptive semiring

Sr.Zero (n)
  returns a square Sr.zero matrix of size n

Sr.One (n)
  returns a square identity matrix of size  n  with  Sr.zero and Sr.one

Sr.adapt (A) 
  replaces  0s  in matrix A with Sr.zero-s

Sr.binary (A) 
  replaces non Sr.zero elements of A with Sr.one

Sr.select  (n,s) 
  returns a vector with  s  Sr.one-s followed by  n-s  Sr.zero-s

Sr.Perm  (A,p) 
  reorders matrix  A  according to the permutation  p return

Sr.perminv  (p)
  returns the inverse permutation of permutation  p


'%[+]%'   (A,B) - function name Sr.Plus
  binary operation on vectors and matrices - extension of semiring operation  %(+)%

'%[.]%' (A,B) Sr.Times 
  returns the product of compatible vectors/matrices

Sr.pow    (A,k)
  returns the k-th power of matrix A
  
Sr.Sumpow (A,k)
  returns the sum of the first k+1 powers of A

Sr.Closure (A)
  returns the closure of the matrix  A  over an absorptive semiring

Sr.save.net (fnet,A)
  saves the matrix  A  to the file  fnet  in Pajek's matrix format

TO DO:

Sr.load.net (fnet)

