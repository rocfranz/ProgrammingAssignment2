## the two function allow to calculate the matrix inversion and to cache the result in case the operation is repeated

## The makeCacheMatrix function creates a list of function to get the data (get), in this case an invertible square 
## matrix, set the solution (setsol) and retrieve it in case has been precedently calculated (getsol)

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
                set <- function(y) {
                        x <<- y
                        s <<- NULL
                }
                get <- function() x
                setsol <- function(sol) s <<- sol
                getsol <- function() s
                list(set = set, get = get,
                     setsol = setsol,
                     getsol = getsol)
}


## The cacheSolve function has to be called on the list created by the makeCacheMatrix function. It retrieves the s variable
## from the cache and in case it is not null it displays it. If the cache is empty it calculates the matrix inversion
## and stores it in the cache.

cacheSolve <- function(x, ...) {
       s <- x$getsol()
                 
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsol(s)
        s
}
