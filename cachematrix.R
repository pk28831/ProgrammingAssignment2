## This file contains 2 functions: MakeCacheMatrix and CacheSolve
##
##
## makeCacheMatrix:
##      creates  a list of functions for probing the matrix and its inverted
##      matrix.
##      Assumption, only regular (non-singular) matrices are forrwarded to the
##      function
## 
##
## CacheSolve:
##      checks whether the matrix is already inverted or not.
##      If the natrix is already inverted, the cached inverted matrix is 
##      returned, otherwise the matrix is inverted by the solve function:
##              solve(a,b) solves the equation AX=B, A,X, and B are matrices
##              if B is not given, B is replaced by the unity matrix I, 
##              solving for X, will result in the inverse of A. 
##
##
##
## By calling makeCacheMatrix:
##      a matrix is read into function orgMatrix$getMatrix and the inverted
##      matrix is cleared and a list of other functions for this matrix is
##      created.
##      The other items of the list are:
##              setMatrix:
##                      By calling orgMatrix$setMatrix(newMatrix) a new matrix
##                      can be read and the inverse is cleared
##      `       setInverse:
##                      By calling orgMatrix$setInverse(inverseMatrix), a new
##                      inverted matrix is read and the orgMatrix$getMatrix is
##                      cleared, as there is no relation with the InverseMatrix
##              getInverse:
##                      By calling orgMatrix$getInverse(), the inverted matrix
##                      is read
##
##
makeCacheMatrix <- function(orgMatrix = matrix()) {
        inv <- NULL
        ##
        setMatrix <- function(newMatrix) {      
                orgMatrix <<- newMatrix 
                inv <<- NULL     
        }
        ##
        getMatrix <- function() orgMatrix
        ##
        setInverse <- function(inverseMatrix){   
                  inv<<- inverseMatrix           
                  orgMatrix<<-NULL 
        }
        ##
        getInverse <- function() inv
        ## actual list with functions
        list(setMatrix = setMatrix,
             getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}
##
##
##
## By calling CacheSolve:
##      The inverted matrix is read from cache if it was calculated previously
##      with the function orgMatrix$getMatrix.
##      If this is not the case (inv is NULL) then the matrix is inverted and 
##      stored in orgMatrix$getInverse.
##
##
cacheSolve <- function(orgMatrix, ...) {
        inv <- orgMatrix$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matri <- orgMatrix$getMatrix()
        inv <- solve(matri, ...)
        orgMatrix$setInverse(inv)
        inv
}
