## This function creates a special "matrix" object that can cache its inverse.
## It includes functions to:
##  - set the value of the matrix
##  - get the value of the matrix
##  - set the value of the inverse matrix
##  - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setimatrix <- function(imatrix) m <<- imatrix
  getimatrix <- function() m
  list(set = set, get = get,
       setimatrix = setimatrix,
       getimatrix = getimatrix)
  
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# But it first checks if the inverse matrix has already been chached.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getimatrix()
  if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setimatrix(m)
  m
}
