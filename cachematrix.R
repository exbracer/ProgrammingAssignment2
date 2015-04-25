## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then 
## cacheSolve should retrieve the inverse from the cache.
##

## Write a short comment describing this function
## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse 
## 4. get the value of the inverse 
## In this function, inv is the value that cache the inverse.
## When a special matrix is created, the inv will not be 
## calculated. 
## When the special matrix is changed, the set() will be called,
## and the inverse will be set NULL for a new calculation.
## 
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
## The following function calculates the inverse of the special
## matrix created with the above function. However, it first 
## checks to see if the inverse has already been calculated. If
## so, it gets the inverse from the cache and skips the 
## computation. Otherwise, when the specail matrix is new or a 
## changed one, it calculates the inverse of the data and set 
## the value of the inverse in the cache via the setinv().
##
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    ## if the inverse is cached, just return the cached one
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    ## if the matrix is new or changed, 
    ## the inverse will be calculated and cached
    message("matrix is new or changed")
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv) 
    inv
}
