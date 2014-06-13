###############################################################################
#
#   cachematrix.R
#   Author: Eugene Jarder
#
#   This file contains functions related to caching the inverse of a matrix. 
#
#   Requirement for R Programming course at Coursera
#
###############################################################################

# makeCacheMatrix()
# Creates a CacheMatrix, a matrix whose inverse is cacheable.
# The user can pass a matrix as a parameter to initialize its value
makeCacheMatrix <- function(thismatrix = matrix())
{
    thisinverse <- NULL
    
    # set the inverse of the CacheMatrix
    set <- function(thatmatrix)
    {
        thismatrix <<- thatmatrix
        thisinverse <<- NULL
    }
    
    # get the inverse of the CacheMatrix
    get <- function() thismatrix
    
    # set the inverse of the CacheMatrix
    setinverse <- function(thatinverse) thisinverse <<- thatinverse
    
    # get the inverse of the CacheMatrix
    getinverse <- function() thisinverse
    
    # make the functions accessible as a list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

# cacheSolve()
# Computes the inverse of the matrix. Uses the solve() built in function
# Users can pass solve()'s optional parameters as well to modify the result.
cacheSolve <- function(cachedmatrix, ...)
{
    thisinverse <- cachedmatrix$getinverse()
    if (!is.null(thisinverse))
    {
        message("Inverse is set! Retrieving cached inverse")
        return(thisinverse)
    }
    thismatrix <- cachedmatrix$get()
    thisinverse <- solve(thismatrix, ...)
    cachedmatrix$setinverse(thisinverse)
    thisinverse
}
