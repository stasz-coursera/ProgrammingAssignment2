## Matrix inversion is usually a costly computation.  The following functions
## enable caching of an inverse matrix, once computed, so that it may be recalled
## rather than re-computed each time

# Written by stasz-coursera

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setInverseMatrix <- function(inverseMatrix) m <<- inverseMatrix
  
  getInverseMatrix <- function() m
  
  list(
    set = set,
    get = get,
    setInverseMatrix = setInverseMatrix,
    getInverseMatrix = getInverseMatrix
    )
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  m <- x$getInverseMatrix()

  ## Check if the matrix was filled from the cache
  if(!is.null(m)) {
    message("Getting cached data...")
    return (m)
  }
  ## Since return wasn't called, the result was not retrieved from the cache
  message("Calculating inverse matrix...")
  data <- x$get()
  
  ## Caclulate the Inverse Matrix (assuming is is square)
  m <- solve(data, ...)
  x$setInverseMatrix(m)
  
  ## Output the resut
  m
}
