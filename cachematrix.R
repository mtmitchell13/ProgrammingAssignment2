## The two functions below, makeCacheMatrix and cacheSolve, cache
## the inverse of a square matrix so it can be computed repeatedly

## makeCacheMatrix creates a square matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve computes the inverse of a square matrix returned by
## makeCacheMatrix; if the inverse has already been calculated then
## cacheSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}