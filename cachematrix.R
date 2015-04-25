## The functions here give the user the ability to work with matrices with c
## cached inverses.
##
## The function makeCacheMatrix and 
## up matrix objects that store a matrix and its inverse. 
## The function cacheSolve accesses the inverse
##
## Calling mat_x<-makeCacheMatrix(x) makes
## mat_x a matrix object with a cached inverse, initially NULL.
## Calling cacheSolve(mat_x) returns the cached inverse of x if it is
## non-NULL, and computes, caches, and returns the inverse if it is 
## initially NULL.


## This function creates a list of functions: 
## get for retrieving the matrix x,
## set for setting the matrix x, 
## setinverse for setting the inverse of the matrix,
## getinverse for retrieving the inverse to date
## This list works as a matrix object with a cached inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
      set <- function(y) {
              x <<- y
              inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)


}


## cacheSolve takes as its argument a matrix object with cached inverse 
## created with the makeCacheMatrix function above. 
## If the cached inverse, inv, is non-NULL, it returns the inverse, 
## with a note that the cached value of the inverse was used.
## If inv is NULL, the inverse of the matrix is computed, cached in inv,
## and returned.

cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data, ...)
	x$setinverse(inv)
	inv
}

