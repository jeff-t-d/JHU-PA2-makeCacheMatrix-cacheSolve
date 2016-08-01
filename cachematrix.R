#################################################################
#
# PGA
# Programming Assignment 2 : Lexical Scoping
# Problem 1
#
# Remember:
#
# The <<- operator is used to assign a value to an object in an
# environment that is different from the current environment.
#
# Computing the inverse of a square matrix can be done
# using solve(). If 'x' is a square matrix, solve(x) will
# return its inverse.
#
#################################################################
#
# makeCacheMatrix() creates a special "matrix" object that
# can cache its inverse.
#
#################################################################
makeCacheMatrix <- function(x) = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}
#################################################################
#
# PGA
# Programming Assignment 2 : Lexical Scoping
# Problem 2
#
# cacheSolve() calculates the inverse of the special "matrix"
# returned by makeCacheMatrix(). If the inverse has already
# been calculated, and the matrix has not changed, then
# cachSolve() will retrieve the inverse from the cache.
#
#################################################################
cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
}
