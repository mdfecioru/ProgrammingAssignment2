## Methods for caching the result of inverting a matrix. 

## Example on how to test / use these functions:
##
## m <- matrix(c(1, 0, 5, 2, 1, 6, 3, 4, 0), nrow = 3, ncol = 3)
## cm <- makeCacheMatrix(m)
## inv<-cacheSolve(cm)
## inv                       # this computes the invese and saves into cache
## inv2<-cacheSolve(cm)
## inv2                      # this gets the inverse from cache

## makeCacheMatrix prvides the set of methods that allow setting/getting
## of a new matrix that needs to be inverted and the functions for
## setting/getting the result of the matrix inversion into a local cache.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y;
        inv <- matrix();
    }
    get <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    list (set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve receives as parameter the result of the makeCacheMatrix and
## returns the inverse of the matrix managed by makeCacheMatrix. This inverse
## is either computed (if not available in cache) or returned from cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
