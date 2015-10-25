## The functions cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setInverse<-function(solve) m<<-solve
  getInverse<-function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function calculates the inverse of the special "matrix" and checks if the inverse 
## was already created.

cacheSolve <- function(x, ...) {
  m<-x$getInverse()
  if(!is.null(m)){
    message("Getting cached data")
    return (m)
  }
  z<-x$get()
  m<-solve(z, ...)
  x$setInverse(m)
  m
}
