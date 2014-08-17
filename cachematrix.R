## The following 2 functions, makeCacheMatrix and cacheSolve,  are created to cache potentially time-consuming computations
## of calculating the inverse of a matrix.
##


## The function, makeCacheMatrix, creates a special "matrix", which is a list containing the following 4 functions:
##  set - set the value of the matrix
##  get - get the value of the matrix
##  setInverse - set the value of the inverse of the matrix
##  getInverse - get the value of the inverse of the matrix
##
##  Example: To initialise a 2x2 matrix and assign it to myMatrix
##  myMatrix <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
##
##  Example: To get the value of the matrix
##  myMatrix$get()
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL  ## this is to hold the value of the inverse matrix
    set <- function(y) {
        x <<- y
        m <<- NULL  ## Whenever a new matrix is loaded, set the inverse matrix to NULL
                    ## as the inverse matrix has not been computed yet 
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    
    list(set = set, get=get, setInverse = setInverse, getInverse = getInverse)
}



## The function, cacheSolve, calculates the inverse of the special "matrix" created with the makeCacheMatrix function.
## Matrix inversion is usually a costly computation and there may be some benefit to caching the result.
##
## The function checks if the inverse has already been calculated.
##      If yes, it skips the calcuation and gets the value from the cache.
##      Otherwise, it calculates the inverse and keeps the result in the cache via the setInverse function.
##
## Limitation: The matrix supplied is always assumed to be invertible.
##
## EXample:
## myMatrix <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
## cacheSolve(myMatrix)
cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if (!is.null(m)) {  ##check if result has been cached 
        message("getting cached data")
        return(m)
    }
    
    data <- x$get()

    if (nrow(data)==ncol(data)) {   ##check if the matrix is a square matrix
        m <- solve(data)    ## if yes, simply use solve() function to inverse the matrix
    } else {
        require(MASS)       ## Otherwise, load MASS package if necessary 
        m <- ginv(data)     ## and use ginv() function, which is computationaly more expensive, to inverse the matrix
    } 
    x$setInverse(m)     ## store the result in the cache
    m   ## Return a matrix that is the inverse of 'x'
}
