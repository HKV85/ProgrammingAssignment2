makematrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


cacheinv <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
t=rnorm(36,25,10);t
m=matrix(t,6);m
# k=makeVector(m);k$get();k$getmean()
# k1=cachemean(k);k1

p=makematrix(m);p$get()
q=cacheinv(p);q
str(q)

