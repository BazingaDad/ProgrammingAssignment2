
## This file contains two functions, makeCacheMatrix and cacheSolve, that
## cache the inverse of a matrix and return the inverse

## makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
        x <<- y
        m <<- NULL
     }
     get <- function() x
     setInvmatrix <- function(InvMatrix) m <<- InvMatrix
     getInvmatrix <- function() m
     list(set = set, get = get, 
          setInvmatrix = setInvmatrix, 
          getInvmatrix = getInvmatrix)
}

## cacheSolve computes the inverseof the special"matrix" returned by makeCacheMatrix
## If inverse has been caculated, cacheSolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        m <- x$getInvmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInvmatrix(m)
        m 
        ## Returns a matrix, m, that is the inverse of x
}