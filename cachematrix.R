
## Functions invert given matrix and cache the inverted version so that it does not need to be recalculated each time it is needen 


## Function makeCacheMatrix creates a cache 'place' for the matrix to be stored 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Function cacheSolve returns invertion of the given matrix.
## Function checks the cache for the inverted version of the given matrix - and restores it from the cache (if previously calculated).
## If the inverted matrix is not yet in the cache (has not yet been calculated) - the function calculates it and puts it in the cache for future use.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
