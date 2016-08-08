# The second function calculates the mean of the special "vector"
# created with makeVector. However, it first checks to see if the
# mean has already been calculated. If it has, it gets the mean
# from the cache and skips the computation. Otherwise, it
# calculates the mean of the data and sets the value of the
# mean in the cache via setmean().
#
# cachemean <- function(x, ...) {
#     m <- x$getmean()
#     if(!is.null(m)) {
#         message("getting cached data")
#         return(m)
#     }
#     data <- x$get()
#     m <- mean(data, ...)
#     x$setmean(m)
#     m
# }
#
# cacheSolve: This function computes the inverse of the special
# "matrix" returned by makeCacheMatrix. If the inverse has already
# been calculated, and the matrix has not changed, then
# cacheSolve() should retrieve the inverse from the cache.
#
# Computing the inverse of a square matrix can be done
# using solve(). If 'x' is a square matrix, solve(x) will
# return its inverse.
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
