## The below functions are part of the Programming Assignment 2 - Lexical Scoping assignment
## The scripts are modeled after the instructions of the assignment
## There are two functions.  One creates an object that stores a matrix and caches its inverse.
## The other retrieves the inverse from the cache

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function computes the inverse of the special "matrix" returned by the above function.
## If the inverse has already been calculated and there is no change in the matrix the below
## function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
  
}
