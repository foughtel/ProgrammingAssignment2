## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## As with the vector example, this function
## creates a list of functions that set, get, set the inverse of, and get
## the inverse of a matrix (x).  invrs is the inverse matrix variable 
makeCacheMatrix <- function(x = matrix()) {
    invrs <- NULL
    set <- function(y) {
        x <<- y
        invrs <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invrs <<- inverse
    getinverse <- function() invrs
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function
## This function checks to see if the inverse matrix has been cached
## If it is found, it returns that inverse matrix.  If it is not located
## the given matrix is inverted and the value is stored


cacheSolve <- function(x, ...) {
    invrs <- x$getinverse()
    if(!is.null(invrs)) {
        message("getting cached data")
        return(invrs)
    }
    mat_to_invert <- x$get()
    invrs <- solve(mat_to_invert, ...)
    x$setinverse(invrs)
    invrs
}

