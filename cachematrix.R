## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve is a function that cache the inverse of the special "matrix" object created with the makeCacheMatrix function, it also return the inverse of that matrix

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    } else {
        y <- x$get()
        m <- solve(y)
        return(m)
    }
}
