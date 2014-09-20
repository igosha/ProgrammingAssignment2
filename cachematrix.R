## Following functions allow to compute inverse of the matrix and store
## the result in cache for further use if needed

## makeCacheMatrix creates a special "matrix" object 
## that stores its original value and inverted matrix.
## The inverted matrix is computed and stored in this object 
## after the cacheSolve() is called.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # At start, set x and clear the cached m.
    set <- function(y) {
        x <<- y
        m <<- NULL
    }

    # Returns original value of matrix.
    get <- function() x
    
    # Saves m in cache.
    setinv <- function(z) m <<- z
    
    # Returns m. This is null when m is not in cache yet.
    getinv <- function() m
    
    # declaration of methods?
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function tries to compute the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Try to get matrix from cache.
    m <- x$getinv()
    if(!is.null(m)) {
        message("Cache hit! Data from cache.")
        return(m)
    }
    
    ## Cache is null, need compute new data.
    message("No cache. Computing new data.")
    ## Get original matrix
    data <- x$get()
    ## Check if inversion is allowable (det)
    if(det(data) == 0) {
        message("Original matrix is singular. Can't be inverted.") 
        return (NaN)
    }
    ## Compute matrix inversion 
    m <- solve(data, ...)
    ## Set the cache
    x$setinv(m)
    return(m)
}

## How to check stuff by example
## Execute:
## m1<-matrix(c(1,2,2,1),ncol=2,nrow=2)
## m2<-makeCacheMatrix(m1)
## cacheSolve(m2)
## cacheSolve(m2)%*%m1
## cacheSolve(m2)


