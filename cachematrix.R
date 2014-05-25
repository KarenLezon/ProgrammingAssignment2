## These functions enable the user to cache the inverse of a matrix
##    to save recalculation if the matrix has not changed.

## makeCacheMatrix returns a list of functions that allows the user to 
##    set a matrix and create the cache varible 'inv' to null,
##    get the current matrix,  set the inverse to the cache variable,
##    and get the value of the inverse from the cache variable

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    ## Function to set the original matrix (indicating a change)
    ##   and initialize the cache variable 'inv' to NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## Function to get the matrix
    get <- function() x
    ## Functions to set the cache varible 'inv'
    ##    and to retrieve the inverse of the matrix
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    ## create list of functions
    list(set = set, 
         get = get,
         setinv = setinv,
         getinv = getinv)
}



## cacheSolve returns a matrix that is the inverse of 
##    the input matrix.  It utilizes a cached value if 
##    it exists.


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    ## Check the cached variable "inv'
    ##   if it has a value, return that value.
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached inverse")
       return(inv)
    }
    ## Otherwise get the latest matrix and produce, cache, 
    ##    and return the inverse
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
