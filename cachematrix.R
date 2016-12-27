## Put comments here that give an overall description of what your
## functions do

## Checks if a matrix can be inverted
isinv <- function (x = matrix, type="original") {
  if (nrow(x) != ncol(x)) {
    if (type == "Original")
      message ("Error: Cannot invert a non Square matrix! Exiting...")
    else
      message ("Error: Inverse of a matrix must be square! Exiting...")
    return(-1)
  }
  if (det(x) == 0) {
    if (type == "original")
      message ("Error: determinant is zero; cannot invert! Exiting...")
    else
      message ("Error: determinant of Inverse must not be zero! Exiting...")
    return(-1)
  }
  return(0)
}


## Creates a special vector (i.e. list) holding functions to set/get a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <<- NULL
  
  set <- function (y) {
    x <<- y
    i <<- NULL
  }
  get <- function () x
  
  
  setinv <- function (inv) {
    #Checks on inv
    if (isinv(inv, "inverse") != 0) {
      return (-1)
    }
    i <<- inv
  }
  getinv <- function () i
  
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## Getting cached matrix inverse or computing one if none exist
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached inverse matrix")
    return(i)
  }
  data <- x$get()
  if (isinv(data, "original") != 0) {
    return (-1)
  }
  i <- solve(data, ...)
  x$setinv(i)
  i
}

