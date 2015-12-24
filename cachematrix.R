## This function Caches the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.
## The function takes an argument X which is an empty matrix as input. It does the following:
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the mean
## 4. get the value of the mean


makeCacheMatrix <- function(x = matrix()) {
  InvMatrix <- NULL
  Set <- function(y) {
    x <<- y
    InvMatrix <<- NULL
  }
  GetMatrix <- function() x
  SetInvMatrix <- function(Inverse)InvMatrix <<- inverse
  GetInvMatrix <- function() InvMatrix
  list(set = set, GetMatrix = GetMatrix,
       SetInvMatrix = SetInvMatrix,
       GetInvMatrix = GetInvMatrix)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  InvMatrix <- x$GetInvMatrix()
  if (!is.null(InvMatrix)) {
    message("getting cached data")
    return(InvMatrix)
  }
  mat <- x$GetMatrix()
  InvMatrix <- solve(mat, ...)
  x$SetInvMatrix(InvMatrix)
  InvMatrix
}
