## Put comments here that give an overall description of what your
## functions do
## Overall goal of this assignment is to write pairs of functions which cache the
## inverse of matrix
## Write a short comment describing this function
## The following function create matrix object that cache it's inverse 
## (which is an invertible square matrix).
makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y){
           x <<- y
           inv <<- NULL
     }
     get <- function()x
     setInverse <- function(inverse) inv <<- inverse
     getInverse <- function() inv
     list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)

}


## Write a short comment describing this function
## cacheSolve function computes the inverse of the special
## "matrix' returned by makeCacheMatrix above.
## If the inverse has been calculated and the matrix has not been
## changed, then the inverse will be retrived from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getInverse()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      mat <- x$get()
      inv <- solve(mat,...)
      x$setInverse(inv)
      inv
}


