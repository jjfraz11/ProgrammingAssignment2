# This file contains functions for creating a list object that can
# cache its inverse and for retrieving or calculating the inverse value.

## makeCacheMatrix - create a list object that wraps a matrix and its
# inverse value. This list can get and set the matrix and its inverse.
#
# makeCacheMatrix takes as arguments:
# self - invertible matrix that should be used when calculating inverse
#
# makeCacheMatrix returns a list with the following properties:
# set - update self with a new invertible matrix and reset the inverse value
# get - return the self matrix
# setInverse - update the value of the inverse
# getInverse - return the inverse value
makeCacheMatrix <- function(self = matrix()) {
    inverse <- NULL
    set <- function(other) {
        self <<- other
        inverse <<- NULL
    }
    get <- function() self
    setInverse <- function(value) inverse <<- value
    getInverse <- function() inverse
    list(set = set, get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}

## cacheSolve - retreive the cached inverse of a cacheMatrix "matrix"
# if it exists, otherwise, calculate the inverse and update the cacheMatrix
# with the new inverse value before returing it.
#
## cacheSolve takes as arguments:
# cacheMatrix - special matrix created using `makeCacheMatrix`
# ... - arguments passed to solve when calculating inverse
#
## cacheSolve returns the inverse of the cacheMatrix
cacheSolve <- function(cacheMatrix, ...) {
    inverse <- cacheMatrix$getInverse
    if(!is.null(inverse)) {
        message("getting cached inverse")
        return(inverse)
    }
    data <- cacheMatrix$get()
    inverse <- solve(data, ...)
    cacheMatrix$setInverse(inverse)
    inverse
}
