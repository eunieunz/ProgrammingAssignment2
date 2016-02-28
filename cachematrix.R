## This function lets you cache the inverse matrix of your original matrix

## This function creates a special "matrix" object that can 
## cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        matInvers <- NULL 
        set <- function(y = matrix()) {
                x <<- y
                matInvers <<- NULL
        }
        get <- function() x
        setInv <- function(solve) matInvers <<- solve
        getInv <- function() matInvers
        list(set = set, get = get, setInv = setInv, getInv = getInv)
        
}


## This function computes the inverse of the special "matrix" 
## returned by makeCache Matrix above.
## If the inverse has already been calculated (and the matrix
## has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        matInvers <- x$getInv()
        if(!is.null(matInvers)) {
                message("getting cached data")
                return(matInvers)
        }
        data <- x$get()
        matInvers <- solve(data, ...)
        x$setInv(matInvers)
        matInvers
        ## Return a matrix that is the inverse of 'x'
}

