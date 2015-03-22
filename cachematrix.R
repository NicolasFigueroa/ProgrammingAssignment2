## The function creates a special matrix to calculate its inverse and stored in cache

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(Inverse) m <<- Inverse
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    

}


## The function checks whether the inverse of the matrix was already calculated. 
## If so, returns the reverse play stored in cache, otherwise the calculated

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
    
    m <- x$getInverse()
    if(!is.null(m)) {
        message("Getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
