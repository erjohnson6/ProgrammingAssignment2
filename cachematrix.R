## Assignmet instructions:
## 
## Write the following functions:
## 
## makeCacheMatrix: This function creates a special "matrix" object that can 
## cache its inverse.
## 
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.
## 

## makeCacheMatrix stores the following different variables:
## 1. x <- The original matrix passed in
## 2. x.inv <- an internal matrix containing the inverse of x. This is 
##    initialized to be NULL until it is calculated for the first time.
## 
## Functions of makeCacheMatrix are the following:
##   * set = Set the original matrix and reset x.inv to NULL
##   * get = return the original matrix
##   * setInv = sets x.inv to the matrix passed in
##   * getInv = returns inverse matrix
##   
## ***NOTE: it is assumed that the matrix passed in is always invertible***



makeCacheMatrix <- function(mat = matrix()) {
     mat.inv <- NULL
     set <- function(y) {
          # Reset mat to a new matrix. Reset mat.inv to NULL
          mat <<- y
          mat.inv <<- NULL
     }
     get <- function() mat
     setInv <- function(inv) mat.inv <<- inv
     getInv <- function() mat.inv
     list(set = set, get = get,
          setInv = setInv,
          getInv = getInv)
}


## cacheSolve makes an initial check to validate if matrix inverse has been
## previously solved and cached. If it has been cached then it returns the 
## cached value else the inverse is calculated, cached, and returned.

cacheSolve <- function(mat) {
     ## Return a matrix that is the inverse of 'x'
     ## 
     ## ***NOTE: it is assumed that the matrix passed in is always invertible***
     
     matInv <- mat$getInv()
     if(!is.null(matInv)) {
          message("Returning cached data")
     } else {
          data <- mat$get()
          matInv <- solve(data)
          mat$setInv(matInv)
     }
     matInv
}
