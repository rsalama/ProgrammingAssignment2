## makeCacheMatrix: Creates a special object that allows us to store the matrix itself and
## its inverse. Expose set/get methods for the matrix and its inverse
## cacheSolve: Manages the output of makeCacheMatrix by storing it inverse if 
## one doesn't exist already

## makeCacheMatrix: 
##  input: square matrix (arguement is assumed to be a square matrix, not checked)
##  output: an object that exposes 4 methods: set/get & setinverse/getinverse
makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  get <- function() x
  setinverse <- function(matinv) minv <<- matinv
  getinverse <- function() minv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve:
##  input: output of makeCacheMatrix()
##  output: inverse
##  Checks to see if the inverse matrix has been set on the object. Sets it
##  if not. Returns the matrix inverse
## For more complicated matrices, one can use MASS
## library(MASS)
## inv <- ginv(mat)  # instead of solve(mat)
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  minv <- x$getinverse()
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  mat <- x$get()
  m <- solve(mat)
  x$setinverse(m)
  m
}
