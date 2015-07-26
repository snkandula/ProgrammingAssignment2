## The purpose of this function is to understand "<<-" operator and 
## use cache results if exists otherwise calcute and cache the result.
## Below two functions are used to create a special object that stores 
## a matrix and caches it's inverse.

## makeCacheMatrix: This function creates a special "matrix", 
## which is really a list containing a function to 
## set, get matrix and to set, get matrix inverse 

makeCacheMatrix <- function(x = matrix()) {
    inverse_x <- NULL
    set <- function(y) {
        x <<- y
        inverse_x <<- NULL
    }
    get <- function() x
    setinverse<- function(inverse) inverse_x <<-inverse
    getinverse <- function() inverse_x
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix" ## created by makeCacheMatrix.
## It first checks,if inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve retrieves 
## the inverse from the cache otherwise calculates the inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse_x <- x$getinverse()
    if (!is.null(inverse_x)) {
        message("getting cached inverse matrix")
        return(inverse_x)
    } 
    data <- x$get()
    inverse_x <- solve(x$get())
    x$setinverse(inverse_x)
    return(inverse_x) 
}
