
## makeCacheMatric creates special matrix that is able to cache it's inversion
##
## cacheSove use matrix created by makeCacheMatric to compute its inversion and store it or 
## to get stored inversion form its cache 


## makeCacheMatrix creates special matrix that can store is't inversion
## it have a list of functions to:
## get, set  matrix
## get, set inversion of matrix
makeCacheMatrix <- function(x = matrix()) {
    
    Inverse <- NULL
    set <- function(y,...) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(i) Inverse <<- i
    getInverse <- function() Inverse
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  
}


## cacheSolve return's a matrix that is the inverse of 'x'
## x must be a special matrix created by makeCacheMatrix
## warning: funcion assumes that x always has an inverse 

cacheSolve <- function(x, ...) {
 
    Inverse <- x$getInverse()
    if(!is.null(Inverse))  return(Inverse)
    
    Inverse <- solve(x$get())
    x$setInverse(Inverse)
    Inverse
}


