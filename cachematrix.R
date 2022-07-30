### To Do: This script consists of 2 functions: makeCacheMatrix and cacheSolve

### Function to generate the special matrix object that holds the input matrix and its inverse counterpart in cache. IT follows the same pattern as the example set. It returns the list of functions to be used in the  cacheSolve function.

makeCacheMatrix <- function(matrix.data = matrix(nrow = 2, ncol = 2)) {
    inv_mat <- NULL # initialization
    
    # set the input values
    setInput <- function(md){
        matrix.data <<- md
        inv_mat <<- NULL
    }
    
    # get the matrix data
    getInput <- function() {
        return(matrix.data)
    }
    
    # use solve function to inverse matrix as the inputs
    setInverseMatrix <- function(solve) {
        inv_mat <<- solve
    }
    
    getInverseMatrix <- function() {
        return(inv_mat)
    }
    
    list(setInput=setInput, getInput=getInput, setInverseMatrix=setInverseMatrix, getInverseMatrix=getInverseMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv_mat <- x$getInverseMatrix()
    
    if(!is.null(inv_mat)){
        message("Accessing the Cached Inverse Matrix")
        return(inv_mat)
    }
    get_data <- x$getInput()
    inv_mat <- solve(get_data, ...)
    x$setInverseMatrix(inv_mat)
    inv_mat
}
