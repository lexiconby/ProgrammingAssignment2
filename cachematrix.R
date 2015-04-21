## R Programming assignment 2
## Written by Billy Yeh

## This function creates the cached matrix
makeCacheMatrix <- function( theMatrix = matrix()) {
    #initialize the inverted matrix we plan on caching to null
    theMatrixInverted <- NULL
    
    #setter of the matrix
    setTheMatrix <- function(theMatrixLocal) {
        theMatrix <<- theMatrixLocal
        #when the matrix changes, the cache will be flushed
        theMatrixInverted <<- NULL
    }
    #getter of the matrix simply returns the matrix
    getTheMatrix <- function() theMatrix
    #setter of inverted matrix is when caching is created
    setTheMatrixInverted <- function(solve) theMatrix
    #getter of inverted matrix simply returns the inverted matrix
    getTheMatrixInverted <- function() theMatrixInverted
    
    list (
        setTheMatrix = setTheMatrix, 
        getTheMatrix = getTheMatrix,
        setTheMatrixInverted = setTheMatrixInverted,
        getTheMatrixInverted = getTheMatrixInverted
    )
}


## This function is the main entry point to invert the matrix
cacheSolve <- function(x, ...) {
    
    theMatrix <- x$getTheMatrix()
    
    theMatrixInverted <- x$getTheMatrixInverted()
    
    if(!is.null(theMatrixInverted)) {
        message("getting cached inverted matrix")
        return(theMatrixInverted)
    } else {
        NULL
    }
    
    
}
