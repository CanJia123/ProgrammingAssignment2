## Put comments here that give an overall description of what your
## functions do


#This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ins <- NULL
  set <- function(y){
  x <<- y
  ins <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) ins <<- inverse
  getInverse <- function() ins 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" 
#returned by makeCacheMatrix above. If the inverse has already been
#calculated (and the matrix has not changed), then the cachesolve
#should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ins <- x$getInverse()
  if(!is.null(ins)){
    message("getting cached data")
    return(ins)
  }
  mat <- x$get()
  ins <- solve(mat,...)
  x$setInverse(ins)
  ins
}
