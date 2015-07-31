## R Programming Language provides a mechinism to cache results of a conputationally
## intensive/expensive function. THe following functions use the <-- operator to cache
## the inverse of a matrix. Given a matrix, the cache is looked up first before the inverse
## operation is done. Call the makeCacheMatrix function first to create a special matrix that 
## manages the cache, followed by the cacheSolve function to get/compute the inverse
## of the desired matrix.


## The makeCacheMatrix creates a special structure that implements the logic for managing
## the cache of the inverse of matrices. It builds a list containing functions to
##
## set the value of the input matrix
## get the value of the input matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x_matrix=matrix()) {
        
        ##Variable for storing the inverse of the matrix
        matrix_inverse <- NULL
        
        ## function to set the value of the input matrix
        setMatrix <- function(y_matrix) {
                x_matrix <<- y_matrix
                matrix_inverse <<- NULL
        }
        
        ## function to set the value of the input matrix
        getMatrix <- function() {x_matrix}
        
        ## function to set the value of the inverse matrix
        setInverse <- function(x_inverse) matrix_inverse <<- x_inverse
        
        ## function to get the value of the inverse matrix
        getInverse <- function() matrix_inverse
        
        ## build the list of setter and getter functions
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)

}


## The cacheSolve function The following function computes inverse of the special "matrix"
## created with the above function. It first checks to see if the inverse has already 
## been calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it computes the inverse and stores it in the cache

cacheSolve <- function(x_matrix, ...) {
        
        ## Return a matrix that is the inverse of 'x_matrix'
        matrix_inverse<- x_matrix$getInverse()
        
        ## Check if the inverse was stored in the cache. 
        ## If yes, retrieve from cache
        if(!is.null(matrix_inverse)) {
                
                message("Getting cached data ...")
                return (matrix_inverse)
        }
        
        ## else compute the inverse using the solve function
        special_matrix_data <- x_matrix$getMatrix()
        matrix_inverse <- solve(special_matrix_data, ...)
        
        ## Store the inverse in the cache
        x_matrix$setInverse(matrix_inverse)
        
        ## return
        matrix_inverse
        
}

