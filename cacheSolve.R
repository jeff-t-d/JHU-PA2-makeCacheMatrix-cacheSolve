# cacheSolve() calculates the inverse of the special "matrix"
# returned by makeCacheMatrix(). If the inverse has already
# been calculated, and the matrix has not changed, then
# cachSolve() will retrieve the inverse from the cache.
#
#################################################################
#
#       cacheSolve(x, ...)
#
#################################################################
#
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
