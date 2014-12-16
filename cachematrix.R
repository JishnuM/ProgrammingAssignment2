## makeCacheMatrix returns a 'matrix' which is implemented using a list
## to set and get the value of the matrix and set and get the value of inverse
## This implementation allows the inverse, once solved, to be cached for
## future calls of cacheSolve

## Returns a 'cacheMatrix' which is implemented using a list
## Allows getting and setting of matrix, getting and setting of inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL ## Variable to hold the cached inverse, if any 
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns the inverse of the cacheMatrix x.
## If the inverse has already been cached, it just returns
## If not, it calculates and caches the inverse before returning

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    # If here, then NULL value in cache
    matrix <- x$get()
    inv <- solve(matrix, ...)
    x$setinverse(inv) # Caching result
    inv
}
