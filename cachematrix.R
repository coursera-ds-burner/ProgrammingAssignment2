## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# make cache matrix creates and returns a list that acts like a class wrapping
# a matrix with setters and getters for the matrix and the inverse, meant to
# act as a matrix with the ability to cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  get <- function() { x }
  set <- function(mat) {
    x <<- mat
    m <<- NULL
  }
  getInverse <- function() { m }
  setInverse <- function(inv) { m <<- inv }
  
  list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}


## Write a short comment describing this function

# cacheSolve uses the previously defined makeCacheMatrix function to solve the
# inverse of a matrix, potentially speeding up the calculating if result is already
# cached
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  
  if (!is.null(i)) {
    print('got cached result')
    i
  }
  
  i <- solve(x$get(), ...)
  x$setInverse(i)
  i
}
