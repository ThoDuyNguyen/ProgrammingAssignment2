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
## cache  

makeCacheMatrix <- function(x = matrix()) {
    cachedInverseMatrix <- NULL
    
    setFunction <- function(y) {
        x <<- y
        cachedInverseMatrix <<- NULL
    }
    
    getFunction <- function() {
        x
    }
    
    setInverseFunction <- function(InverseMatrix) {
        cachedInverseMatrix <<- InverseMatrix
    }
    
    getInverseFunction <- function() {
        cachedInverseMatrix
    }
    
    list(set = setFunction, get = getFunction,
         setInverseMatrix = setInverseFunction,
         getInverseMatrix = getInverseFunction)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    InverseMatrix <- x$getInverseMatrix()
    
    if(!is.null(InverseMatrix)) {
        message("getting cached data")
        return(InverseMatrix)
    }
    
    data <- x$get()
    InverseMatrix <- solve(data, ...)
    x$setInverseMatrix(InverseMatrix)
    InverseMatrix
}
