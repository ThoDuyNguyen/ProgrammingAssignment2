## These 2 functions below are used to calculate and cache 
## inverse matrix of an invertible matrix. After inverse matrix 
## is calculated once, it is cached and store on the memory. 
## Therefore, when inverse matrix is accessed (called) again, the cache 
## value is return and the value is not re-calculated. 
## Processing time of computing is reduced (if inverse matrix 
## of a same an invertible matrix is accessed multiple times) but 
## more memory is used.  


## This function create an object to store a matrix 
## and it's inverse matrix (if the matrix is invertible).
## Initially, when a matrix is created, the inverse matrix is 
## not computed. Also if the data (original matrix) is changed, 
## "cached" is cleared

makeCacheMatrix <- function(x = matrix()) {
    ## This is the cache of inverse matrix
    cachedInverseMatrix <- NULL
    
    ## Set data for the matrix
    ## Since then, the "cache" inverse matrix of it also need
    ## to be cleared
    setFunction <- function(y) {
        x <<- y
        cachedInverseMatrix <<- NULL
    }
    
    ## Get the matrix 
    getFunction <- function() {
        x
    }
    
    ## Set new value for the the cached inverse matrix
    setInverseFunction <- function(InverseMatrix) {
        cachedInverseMatrix <<- InverseMatrix
    }
    
    ## Get value of inverse matrix
    getInverseFunction <- function() {
        cachedInverseMatrix
    }
    
    ## Private methods is mapped to public so that
    ## we could called them    
    list(set = setFunction, get = getFunction,
         setInverseMatrix = setInverseFunction,
         getInverseMatrix = getInverseFunction)
}


## Compute the inverse matrix of a matrix object
## The inverse is stored on the cache of the matrix 
## object

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    InverseMatrix <- x$getInverseMatrix()
    
    ## If the inverse matrix is cache, just return it,
    ## there is no need to compute it again. 
    ## A message is also printed out to notify user. 
    if(!is.null(InverseMatrix)) {
        message("getting cached data")
        return(InverseMatrix)
    }
    
    ## The inverse matrix is not cached yet. 
    ## Compute and then cache inverse matrix 
    ## for further use
    data <- x$get()
    InverseMatrix <- solve(data, ...)
    x$setInverseMatrix(InverseMatrix)
    InverseMatrix
}
