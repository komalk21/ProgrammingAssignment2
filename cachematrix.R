## Assignment: Caching the Inverse of a Matrix

## writing a pair of functions that cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set=set, get=get, 
       setsolve = setsolve,
       getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}

# testing if the above function work
Mat <- matrix(c(2, 3, 4, 1),
              nrow = 2,
              ncol = 2,
              byrow = TRUE)

Mat

mat1 <- makeCacheMatrix(Mat)

cacheSolve(mat1)
cacheSolve(mat1)
