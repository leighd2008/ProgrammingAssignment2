## These functions will create a special "matrix" that can compute and cache
## the inverse of a matrix.

## The makeCacheMatrix function will create a special "matrix" that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## Create a matrix 'x' that can cache its inverse
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<-solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## The cachesolve function computes the inverse of the special "matrix" created 
## by the makeCacheMatrix function above. If the inverse has already been 
## calculated (and the matrix has not changed), cachesolve will retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'!
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
