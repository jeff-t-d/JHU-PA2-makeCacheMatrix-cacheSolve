# makeCacheMatrix() creates a special "matrix" object that
# can cache its inverse.
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
# makeCacheMatrix(x)
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
