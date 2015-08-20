  # `makeCacheMatrix`: This function creates a special "matrix" object
  # that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # Creates a special "matrix" object
  #
  # Arg:
  #  x: An invertible matrix (assumed for the assignment)
  #
  # Return:
  #  A list of functions to be used by cacheSolve:
  #   1.  set the matrix          / set()
  #   2.  get the matrix          / get()
  #   3.  set the inversed matrix / setinverse()
  #   4.  get the inversed matrix / getinverse()
  inversed.matrix <- NULL

  set <- function(y) {
    x <<- y  # Cache the matrix
    inversed.matrix <<- NULL
  }

  get <- function() {
    x
  }

  setinverse <- function(inversed) {
    # Cache the inversed matrix
    inversed.matrix <<- inversed
  }

  getinverse <- function() {
    # Get the inversed matrix
    inversed.matrix
  }

  list(set        = set,
       get        = get,
       setinverse = setinverse,
       getinverse = getinverse)

}

  # `cacheSolve`: This function computes the inverse of the special
  #  "matrix" returned by `makeCacheMatrix` above. If the inverse has
  #  already been calculated (and the matrix has not changed), then
  #  `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  # computes the inverse of the special "matrix"
  # returned by `makeCacheMatrix`
  inversed.matrix <- x$getinverse()

  if (!is.null(inversed.matrix)) {
    # Checks if the inversed matrix is cached
    # return inversed matrix and skips new computation
    message("getting cached matrix")
    return(inversed.matrix)
  }

  # Else compute the reversed matrix and cache it through setinverse() 
  matrix.data <- x$get()
  inversed.matrix <- solve(matrix.data)
  x$setinverse(inversed.matrix)
  inversed.matrix

}
