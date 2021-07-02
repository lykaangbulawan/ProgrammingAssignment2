## Put comments here that give an overall description of what your
## functions do

## The makeCacheMatrix is the primary function.
## In this function the <<- operator is used.
## In this function it consists of set, get, setInverse, and getInverse.

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## The function in this section is used to get cached data.

cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv ## Return a matrix that is the inverse of 'x'
}
