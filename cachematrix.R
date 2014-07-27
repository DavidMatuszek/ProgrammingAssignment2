## To use these methods:
## my.matrix <- makeCacheMatrix(m) will return a memoizable copy of matrix m.
##    Now my.matrix$"get"() will return the copy of m, and my.matrix$"getinverse"()
##    will still return NULL. You shouldn't call my.matrix$"setinverse".
## cacheSolve(my.matrix) will now return the inverse of m.
## cacheSolve(my.matrix) called again will print a message and return the same inverse.

## makeCacheMatrix(m) returns a structure containing a copy of matrix m and some
## functions to operate on m. The only "user friendly" function is get(), which
## will return the contained matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(mean) m <<- mean
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## When called with a structure cm created by makeCacheMatrix, cacheSolve(cm) will
## return a matrix that is the inverse of that matrix contained in cm.
## Note that since cacheSolve returns a matrix, not a structure containing a matrix,
## cacheSolve(cacheSolve(cm)) gives an error rather than returning the original matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
