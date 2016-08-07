## Functions 'makeCacheMatrix' and 'cacheSolve' return the inverse of a matrix either by calculation or if already
## calculated using the caching capability of R.  

## 'makeCacheMatrix' function takes an invertible matrix as an input and outputs a list containing functions to
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse
## 4. get the inverse

makeCacheMatrix <- function(x = matrix()) {
    
    # Initialising the object 'inverseMatrix' as an empty object
    inverseMatrix <- NULL
    
    # 'set' function caches the matrix y to the object x
    set <- function(y){
        x <<- y
        inverseMatrix <<- NULL
    }
    
    # 'get' function returns the matrix
    get <- function() x
    
    # 'setInverse' function caches the 'inverse' object in 'inverseMatrix'
    setInverse <- function(inverse) inverseMatrix <<- inverse
    
    # 'getInverse' function returns the inverseMatrix
    getInverse <- function() inverseMatrix
    
    # returns the list of functions
    list(set=set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## 'cacheSolve' function takes the output from 'makeCacheMatrix function as input and returns the 
## the inverse of the matrix that was the input to 'makeCacheMatrix' function

cacheSolve <- function(x, ...) {
        
    # Obtaining the inverseMatrix from 'makeCacheMatrix'function
    inverseMatrix <- x$getInverse ()
    
    # If matrix x is not cached, then the 'inverseMatrix'is null. If matrix x is cached
    # then the 'inverseMatrix'is retrieved from the cache.
    if(!is.null(inverseMatrix)) {
        message ("getting cached data")
        return (inverseMatrix)
    }
    
    data = x$get()
    inverseMatrix = solve(data,...)
    
    # caching the inverse of the matrix 
    x$setInverse (inverseMatrix)
    
    return(inverseMatrix)
    
}
