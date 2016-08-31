## Caclulate the inverse of a matrix with cache. if the result is in cache 
## get the result from cache. But the result is not in cache, get the result 
## with `solve` function and save the result in cache. 

## This function creates some special matrix class which contains `x` the 
## origin matrix, `i` cached invers matrix of `x` and methods getter and 
## setters of `x` and `i`. 
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) i <<- inv
    getInverse <- function() i
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Cacluates the inverse of a matrix using `makeCacheMatrix`. 
## If it has aleady been caculated before, it can be retrieved from the cache. 
## If it's not in the cache, calculate it using `solve` function and cache it.
cacheSolve <- function(x, ...) {
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
