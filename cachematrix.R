## Pair of Functions that cache the inverse of a matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  get <- function() x
  setinv <- function()  m <<- solve(x)
  getinv <- function() m
  inv <- function(inverse) m <<- inverse
  list( get = get,setinv = setinv,
        getinv = getinv, inv = inv)
}



## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$inv(m)
  m
} 
