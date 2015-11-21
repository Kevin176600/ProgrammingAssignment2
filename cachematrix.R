## Together these function provide a way to cache the inverse of matrix to avoid recomputing the inverse repeatedly.
## Usage:
## m <- matrix(c(2,3,4,5,6,7,8,9,1),nrow=3,ncol=3)
## cm <- makeCacheMatrix(m)
## cacheSolve(cm)   - an initial call creates an inversed matrix and caches it
## cacheSolve(cm)   - subsequent calls return the cached inversed matrix

## makeCacheMatrix - A function to cache the inverse of a matrix and provide a list of methods for accessing that inversed matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    setmatrix <- function(y){
        x <<- y
        m <<- NULL
    }
    getmatrix <- function() {x}
    setinverse <- function(inverse) { m <<- inverse }
    getinverse <- function() { m }
    list(setmatrix = setmatrix, getmatrix  = getmatrix, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve - A function to provide access to a cached matrix or to create a cached matrix if one does not exist.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inverse <- x$getinverse()
    if( !is.null(inverse) ) {
        message("Getting cached matrix")
        return(inverse)
    }
    m <- x$getmatrix()
    inverse <- solve(m, ...)
    x$setinverse(inverse)
    inverse
}
