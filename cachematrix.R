## Fast matrix inversion
## Author: Jack Gidding
## 
## Matrix inversion can be costly in terms of CPU time for larger 
## matricies. The functions in this file can create a special matrix
## that cache the inverse of the matrix once it is calculated. This
## allows other R code to quickly get the inverse without having to 
## recompute it each time it is needed.

## Function: makeCacheMatrix
## Args: x: An R matrix type (default is an empty matrix)
## Purpose: makeCacheMatrix creates a special matrix object that 
##          stores the inverse of the matrix in the variable invm. 
##          The real matrix is stored in the variable x. Functions 
##          setinvs and getinvs will set and get the stored matrix 
##          inverse respectively.  
## Notes:   x must be a square matrix (invertible)

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function(matx) {
    x <<- matx
    invm <<- NULL
  }
  get <- function() x
  setinvs <- function(solve) invm <<- solve
  getinvs <- function() invm
  list(set=set, get=get,
       setinvs=setinvs,
       getinvs=getinvs)
}


## Function: cacheSolve
## Args: x: An R matrix type 
## Purpose: cacheSolve returns the inverse of the matrix. If it
##          has not been caclculated, it will be calculated
##          and cached. If it has been previously calculated, 
##          the function will return the matrix inverse from the
##          cached value and notify the user. 
##          
## Notes:   x must be a square matrix (invertible)

cacheSolve <- function(x, ...) {
        
    invm <- x$getinvs()
    if(!is.null(invm)) {
    message("getting cached data")
    return(invm)
    }
    data <- x$get()
    invm <- solve(data, ...)
    x$setinvs(invm)
    invm
}
