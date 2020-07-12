## Ben Moring 12/07/2020

## The pair of functions makeCacheMatrix and cacheSolve attempts to minimise
## costly matrix inversion calculation when a cached (previous) calculation
## has already been performed. If there is no cached inverse, or a different
## matrix is used, then the inverse calculation is performed as per normal. If
## there is a cached inverse, then it is returned

## The makeCacheMatrix function has one argument which is a matrix with default
## setting of null matrix. A list is returned which houses 3 functions that
## are used on the matrix
## getMatrix simply returns the input matrix
## setInverse essentially caches the inverse of the matrix by setting the
## invMatrix variable in a different environment than the function is called
## (lexical scoping)
## getInverse function simply returns the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
      oldMatrix      <- NULL
      invMatrix      <- NULL
      
      setOldMatrix    <- function(mtrx) oldMatrix <<- mtrx
      getOldMatrix    <- function() oldMatrix
      getMatrix       <- function() x
      setInverse      <- function(mtrx) invMatrix <<- mtrx
      getInverse      <- function() invMatrix
      
      list(setOldMatrix  = setOldMatrix,
           getOldMatrix  = getOldMatrix,
           getMatrix     = getMatrix, 
           setInverse    = setInverse, 
           getInverse    = getInverse)
}


## The cacheSolve function calls the getInverse function from makeCacheMatrix
## and if there is a cached version, it prints a message to screen to indicate
## a cached inverse exists and then prints it. Else, the input matrix is inverted
## and through lexical scoping, the inverse is set through the setInverse
## function. The inverse is also printed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      invMatrix <- x$getInverse()
      oldMatrix <- x$getOldMatrix()
      Matrix <- x$getMatrix()

      if(!is.null(invMatrix) & !is.null(oldMatrix) & identical(Matrix, oldMatrix)) {
          message("getting cached inverse matrix")
          return(invMatrix)
      }
      
      x$setOldMatrix(Matrix)
      invMatrix <- solve(Matrix, ...)
      x$setInverse(invMatrix)
      invMatrix
}