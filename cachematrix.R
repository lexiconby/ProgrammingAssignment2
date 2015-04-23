## R Programming assignment 2
## Written by Billy Yeh

## This function leverages the getter/setter programming concept
## to also be able to create a cached inverse of the matrix passed in
## Note that this we can only accept a square matrix (same rows and columns)
makeCacheMatrix <- function( theMatrix = matrix()) {
    
    # make sure it is a matrix
    if(!is.matrix(theMatrix)) {
        message("Please pass in a valid matrix")
        return
    }
    
    theMatrixInv <- NULL
    
    # setter of the matrix
    setTheMatrix <- function(theMatrixLocal) {
        # whenever matrix is set if it is different from before
        # then we should re-calculate the inverse
        theMatrix <<- theMatrixLocal
        
        #when the matrix changes, assume we need to solve again
        theMatrixInv <<- solve(theMatrixLocal)
    }
    # getter of the matrix
    getTheMatrix <- function() theMatrix

    
    # getter of inverse matrix, no need for a setter of inverse since should be read-only
    getTheMatrixInv <- function(theMatrixLocal) {
        
        theMatrixInv <<- solve(theMatrixLocal)
    }
    # return the list of functions
    list (
        setTheMatrix = setTheMatrix, 
        getTheMatrix = getTheMatrix,
        getTheMatrixInv = getTheMatrixInv
    )
}


## This function will return the inverse of a square matrix
## To use this function, first pass a matrix into the function makeCacheMatrix
## The output of that function is a list of functions to be passed in as the x parameter
cacheSolve <- function(x, ...) {
    
    # get the matrix
    theMatrix <- x$getTheMatrix()

    if(!is.null(theMatrixInv)) {
        message("getting cached inverse matrix")
        return(theMatrixInv)
    } else {
        message("not getting cached inverse matrix")
        theMatrixInv <- x$getTheMatrixInv(theMatrix)
        return (theMatrixInv)
    }
    
    
}

