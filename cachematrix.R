## These functions can be used to cache the inverse of a matrix.
## makeCacheMatrix creates a function object which can be used by cacheSolve
## to store the matrix inverse if it has previously been computed.

## This function creates a closure to store a matrix and its
## inverse. It returns a list of named sub-functions 
## which can be used to both get and set the matrix as well as its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) {
        inv <<- inverse
    }
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse, 
         getinverse = getinverse)
}


## This function uses a cache to efficiently return the inverse 
## of the matrix in a makeCacheMatrix object. 

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    ## If the inverse has been cached, print a message 
    ## and return the cached inverse.
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    # Otherwise, caculate the inverse of the object's matrix, store it 
    # in the input object's cache and return it. 
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
