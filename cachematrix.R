## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
