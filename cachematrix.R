## Reduces the computation time for a matrix inversion by
## caching previously calculated values and only calculating uncached values.
## Matrix must be square and invertible

## Creates a cache matrix for a square invertible matrix
makeCacheMatrix <- function(x = matrix()) {
    ## Initialize the matrix inverse
    i <- NULL

    ## Function to change the matrix and reset the matrix inverse
    set <- function(y) {
        x <<- y
        i <<- NULL
    }

    ## Function to retrieve the matrix
    get <- function() x

    ## Function to update the matrix inverse
    setinverse <- function(solve) {
        i <<- solve
    }

    ## Function to retrieve the matrix inverse
    getinverse <- function() {
        i
    }

    ## Define accessible functions and names
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}

## Returns the the inverse matrix (either newly calculated or from cache)
## of a square invertible matrix
cacheSolve <- function(x, ...) {
    ## Retrieve current matrix inverse of x
    i <- x$getinverse()

    ## If matrix inverse exists, return cached value
    ## otherwise retrieve matrix, calculate the matrix inverse,
    ## and save the matrix inverse
    if(!is.null(i)) {
        message("getting cached data")
    } else {
        i <- solve(x$get(), ...)
        x$setinverse(i)
    }

    ## Return the matrix inverse
    i
}
