## These functions cache the inverse of a matrix.

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {             ## makeCacheMatrix is a function that takes in a single argument x, which is a matrix
        m <- NULL                                       ## Initially sets value of te inverse m to NULL
        set <- function(y) {                            ## Set is a function that takes in a single argument y
                x <<- y                                 ## x is given value y, valid outside this environment
                m <<- NULL                              ## m is given value NULL, valid outside this environment
        }
        get <- function() x                             ## The function get has no arguments and returns x
        setinverse <- function(solve) m <<- solve       ## setinverse is a function with 1 argument, solve, It sets m to solve
        getinverse <- function() m                      ## The function getinverse has no arguments and returns m
        list(set = set, get = get,                      ## returns a list of the functions
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the special "matrix" returned by the 
## makeCacheMatrix function above. If the inverse has already been calculated, then cacheSolve
## will retrieve the inverse from the cache instead of recalculating.
cacheSolve <- function(x, ...) {                        ## cacheSolve is a function 
        m <- x$getinverse()                             ## sets this value of m to the getinverse value from makeCacheMatrix
        if(!is.null(m)) {                               ## if this value of m isn't NULL, this loop prints the message "getting cached data" 
                message("getting cached data")          ## and returns the cached m value
                return(m)
        }
        data <- x$get()                                 ## If m is NULL, this will obtain the matrix from the makeCacheMatrix function and assign it to the variable data
        m <- solve(data, ...)                           ## USes solve find the inverse of the matrix and assigns it to the variable m
        x$setinverse(m)                                 ## Assigns the inverse to setinverse
        m                                               ## Returns the inverse m
}
