## Put comments here that give an overall description of what your
## functions do
## Week3- Assignment November 2015
## Functions that cache the inverse of a matrix

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    xreverse <- NULL 
    # Set the value of the matrix
    set <- function(xin) {
        x <<- xin
        xreverse <<- NULL
    }
    # Get the value of the matrix
    get <- function() x
    # Set the value of the reverse of the matrix
    setxreverse <- function(xreversein) xreverse <<- xreversein 
    # Get the value of the reverse of the matrix
    getxreverse <- function() xreverse
    # Return a list 
    list(set = set, get = get,
         setxreverse = setxreverse,
         getxreverse = getxreverse)
}


## Write a short comment describing this function
## Creates a reversed matrix if it does not exist in cache  

cacheSolve <- function(x, ...) {
    ## Retrieve the cached reversed matrix
    xreverse <- x$getxreverse()
    ## Check to see if the matrix exists 
    if(!is.null(xreverse)) {
        message("Getting cached reversed matrix")
        ## If the reversed matrix exists,m it gets it from the cache and skips the reverse
        return(xreverse)
    }
    ## If the reversed matrix does not exist it gets the matrix
    data <- x$get()
    ## Set the new reversed matrix
    xreverse <- solve(data)
    ## Set the value in the cache via setxreverse function
    x$setxreverse(xreverse)
    ## Return a matrix that is the inverse of 'x'
    xreverse
}

## Run code with
## x <- matrix (c(1:4),2)
## m = makeCacheMatrix(x)
## m$get()
## [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## > ## First call
## > cacheSolve(m)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(m)
## Getting cached reversed matrix
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5