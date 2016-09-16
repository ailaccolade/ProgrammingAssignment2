makeCacheMatrix <- function(x = matrix()){
  m = NULL
  set = function(y) {
    x <<- y
    m <<- NULL
  }
  get<- function() x
  setinv<- function(inv) m <<- inv 
  getinv<- function() m
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  if (!is.null(inv)){
    return(inv)
  }
  mat.data = x$get()
  inv = solve(mat.data, ...)
  x$setinv(inv)
  return(inv)
}