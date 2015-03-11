## Functions to create a matrix that can cache its inverse and to
## calculate (or retrieve) the inverse of a matrix. Both functions
## are closely modeled after the "makeVector" and "cachemean" functions
## described in the assignment instructions.

## Creates a matrix object capable of caching information about
## its inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL      ## variable to store the inverse matrix
        
        ## replaces old matrix and resets inverse to NULL
        set  <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## returns the current matrix (i.e. current value of x)
        get  <- function() {x}
        
        ## sets a new value for the inverse matrix
        setInv <- function(inverse) {inv <<- inverse}
        
        ## returns the current inverse matrix
        getInv <- function() {inv}
        
        ## return the list of functions for future use
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Returns the inverse of a matrix. If the inverse matrix was
## previously calculated (and the matrix hasn't changed since),
## simply returns the cached inverse matrix. Otherwise, calculates
## (and caches) the inverse matrix, and returns the result.
## Assumes that the 'x' will be invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## If there's already a cached inverse matrix, just
        ## return that directly
        inv <- x$getInv()
        if (!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        }
        
        ## Otherwise, calculate the inverse of 'x', cache it,
        ## then return the newly computed inverse matrix.
        matrix <- x$get()
        inv <- solve(matrix)
        x$setInv(inv)
        
        inv
}
