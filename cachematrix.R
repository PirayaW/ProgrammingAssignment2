## These two functions are used to create a special object that 
## stores a matrix and cache's its inverse.


## This function creates a special matrix that can cache its inverse.
## The special matrix is a list containing the following functions:
##  - set: set the matrix and reset the inverse matrix 
##  - get: return the matrix
##  - setSolve: set the inverse matrix
##  - getSolve: return the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL 
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setSolve <- function(solve) s <<- solve
    getSolve <- function() s
    list(set = set, get = get, 
         setSolve = setSolve, getSolve = getSolve)
}


## This function computes the inverse of the special matrix returned by 
## makeCacheMatrix. 
## 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.
##
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
    
    s <- x$getSolve()
    if(!is.null(s)) {
        message("getting cached data")
        return (s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setSolve(s)
    s
}
