# Two functions are defined in this file:
#   1.makeCacheMatrix: This function creates a special "matrix" object that can 
#                      cache its inverse.
#   2.cacheSolve: This function computes the inverse of the special "matrix" 
#                 returned by makeCacheMatrix function.
# Usage:
#   source("cachematrix.R")
#   inputMatrix <- matrix(c(0,1,2,3,4,10,11,12,13),nrow=3,ncol=3)
#   cacheMatrix <- makeCacheMatrix(inputMatrix)
#   cacheSolve(cacheMatrix) # No cache hit
#   cacheSolve(cacheMatrix) # Cache hit
#   cacheSolve(cacheMatrix) # Cache hit again
#
#   inputMatrix <- matrix(c(9,1,2,3,4,10,11,12,13),nrow=3,ncol=3) # Change input
#   cacheMatrix <- makeCacheMatrix(inputMatrix)
#   cacheSolve(cacheMatrix) # No cache hit
#   cacheSolve(cacheMatrix) # Cache hit


# Creates a special "matrix" object that can cache its inverse.
#
# Args:
#   x: the input matrix.
makeCacheMatrix <- function(x) {
    inversed <- NULL
    set <- function(y) {
        x <<- y
        inversed <<- NULL
    }
    get <- function() x
    setInversedMatrix <- function(inversedMatrix) inversed <<- inversedMatrix
    getInversedMatrix <- function() inversed
    list(set = set, get = get,
         setInversedMatrix = setInversedMatrix,
         getInversedMatrix = getInversedMatrix)
}


# Computes the inverse of the special "matrix" returned by makeCacheMatrix 
# function. 
# 
# If the inverse has already been calculated (and the matrix has not changed),
# this function retrieves the inverse from the cache.
#
# Args:
#   x: the special "matrix" object that is created from makeCacheMatrix function.

# Returns:
#   The inversed version of the input matrix.
cacheSolve <- function(x) {
    inversedMatrix <- x$getInversedMatrix()
    if(!is.null(inversedMatrix)) {
        message("Cache hit - return the cached inverse matrix")
        return(inversedMatrix)
    }
    message("No cache hit - create the inversed matrix now")
    originalMatrix <- x$get()
    inversedMatrix <- solve(originalMatrix)
    x$setInversedMatrix(inversedMatrix)
    inversedMatrix
}
