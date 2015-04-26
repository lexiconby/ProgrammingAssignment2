## R Programming assignment 2
## Written by Billy Yeh

## This function leverages the getter/setter programming concept
## to also be able to create a cached inverse of the matrix passed in
## Note that this we can only accept an invertable square matrix (same rows and columns)
makeCacheMatrix <- function( mat = matrix()) {
    
    # initialize inverse matrix
    matInv <- NULL
    
    # setter of the matrix
    setMat <- function(mat2) {
        mat <<- mat2
    }
    
    # getter of the matrix
    getMat <- function() mat

    # setter for the inverse matrix
    setMatInv <- function(matInv2) {
        matInv <<- matInv2
    }
    
    #getter for the inverse matrix
    getMatInv <- function() matInv
    
    # return the list of functions
    list (
        setMat = setMat, 
        getMat = getMat,
        setMatInv = setMatInv,
        getMatInv = getMatInv
    )
}


## This function will return the inverse of a square matrix
## To use this function, first pass a matrix into the function makeCacheMatrix
## The output of that function is a list of functions to be passed in as the x parameter
## The first time you run this function, nothing is cahced yet
## The second time you run this function, theMatrixInv will be cached
cacheSolve <- function(x, ...) {
    
    # get the matrix and inverse
    mat <- x$getMat()
    matInv <- x$getMatInv()
    
    # check to see if we can use cache
    if(!is.null(matInv)) {
        message("getting cached inverse matrix")
        return(matInv)
    } else {
        message("not getting cached inverse matrix")
        matInv <- solve(mat)
        x$setMatInv(matInv)
        return (matInv)
    }
    
}

