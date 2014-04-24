# 2 functions to assist in caching the inverse of a matrix

# TODO: 
#       write guard clause against x != matrix

makeCacheMatrix <- function(x = matrix()) {
    # Wrap the matrix object provided and allow the caching of it's inverse 
    #
    # Args:
    #   x: a matrix object whose inverse can be calculated 
    #
    # Returns:
    #   A list of four functions (set, get, setInverse, getInverse)
        
    inverse <- NULL
    
    # replace the matrix with a new one
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    # get the matrix object
    get <- function() x
    
    # set the inverse of the matrix
    setInverse <- function(inverseMatrix) inverse <<- inverseMatrix
    
    # get the saved inverse or NULL
    getInverse <- function() inverse
    
    # return object
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
    # Compute the inverse of the matrix provided by the makeCacheMatrix object
    #
    # Args:
    #   x: makeCacheMatrix object  
    #
    # Returns:
    #   The inverse of the provided matrix
    
    inverse <- x$getInverse()
    
    # Returns the cached inverse if it is available
    if(!is.null(inverse)) {
        message("getting cached inverse matrix")
        return(inverse)
    }
    
    # calculate the inverse
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    
    # return the inverse
    inverse
}