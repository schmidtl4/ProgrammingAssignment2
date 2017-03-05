## this script makes it possible to calculate and store
## the inverse of a matrix and, if previously calculted,
## to recall the cached value rather than recalculate it

## function to cache inverse of a matrix
makeCacheMatrix <- function(x=matrix()) {

      #assumes matrix is always invertible

            ix <- NULL
      
      set <- function(y) {
            x <<- y
            ix <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) ix <<- solve
      getinverse <- function() ix
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## return a matrix that is the inverse of 'X'; 
## compute the inverse of matrix returned by 
## makeCacheMatrix if not previously cached
cacheSolve <- function(x,...) {
      ix <- x$getinverse()
      if(!is.null(ix)) {
            message("getting cached inverse")
            return (ix)
      }
      data <- x$get()
      ix <- solve(data,...)
      x$setinverse(ix)
      ix
}