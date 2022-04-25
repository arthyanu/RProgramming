## Put comments here that give an overall description of what your
## functions do

## Create a function makeCacheMatrix to store the matrix data
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  ## Create a function set to assign the matrix values
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## Create a function get to get the matrix values
  get <- function() x
  
  ## Create a function setInverse to store the inverse of the matrix
  setInverse <- function(inverse) m <<- inverse
  
  ## Create a function getInverse to get the inverse of the matrix
  getInverse <- function() m
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Create a function cacheSolve to find the inverse of matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## If the inverse of the matrix is already computed the return m
  if(!is.null(m)) {
    message("Getting Cached Data")
    return(m)
  }
  
  ## If not, compute the inverse of the matrix
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

ip1 <- matrix(c(11:20),2,2)
ip2 <- makeCacheMatrix(ip1)
cacheSolve(ip2)

