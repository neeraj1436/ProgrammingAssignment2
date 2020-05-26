## A pair of functions that cache the inverse of a matrix


## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse property
  i <- NULL
  ## Method to set the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## Method to get the matrix
  get <- function()
  {
    x
  }
  ## Method to set the inverse of matrix
  setInverse <- function(inverse)
  {
    i <<- inverse
  }
  ## Method to get the inverse matrix
  getInverse <- function() 
  {
    i
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  }
  
}
## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  ## Just return the inverse if its already set
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  ## Get the matrix from our object
  d <- x$get()
  ## Calculate the inverse using matrix multiplication
  i <- solve(d) %% d
  ## Set the inverse to the object
  x$setInverse(i)
  ## Return the matrix
  i
}
