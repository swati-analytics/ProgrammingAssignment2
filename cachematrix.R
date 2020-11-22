## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix (function)
## --------------------------
## Creates a list containing functions that
## * set the values from input matrix
## * get the values of currently stored matrix
## * set the values of the inverse matrix
## * get the values of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    ## function to set the matrix value
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## function to get the currently stored matrix
    get <- function() x
    
    ## function to set the inverse to the passed value
    setInv <- function(inverse) inv <<- inverse
    
    ## function to get the currently stored inverse
    getInv <- function() inv
    
    ## returns a list of functions
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}

## cacheSolve (function)
## --------------------------
## This function calculates the inverse of the matrix, stored
## in the list that is created with the given function
## * set the values from input matrix
## * get the values of currently stored matrix
## * set the values of the inverse matrix
## * get the values of the inverse matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinv()
    ## First checks to see if the inverse has already been calculated.
    if (!is.null(inverse)) {
        ## If so, it gets the inverse from the cache and skips the computation. 
        message("getting cached data")
        return (inverse)
    }
        
    ## Otherwise, it calculates the inverse of the matrix
    data <- x$get()
    inverse <- ginv(data, ...)
    ## and sets the value of the inverse in the cache
    x$setinv(inverse)
    inverse
}
