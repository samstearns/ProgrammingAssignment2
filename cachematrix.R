

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  # initialize
  m <- NULL
  
  # define function to set
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # define get function
  get <- function() x
  
  # define function to set inverse
  setinverse <- function(solve) m <<- solve

  # define function to get the inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # Computing the inverse of a square matrix can be done with the solve function in R. 
  # For example, if X is a square invertible matrix, then solve(X) returns its inverse.
  # For this assignment, assume that the matrix supplied is always invertible.
  
  inverse <- x$getinverse()
  
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

# Test Data: set up a simple 2x2 matrix
seq1 <- seq(1:4)
mat1 <- matrix(seq1, 2)


# Translate into the matrix object, and find the inverse
foo <- makeCacheMatrix(mat1)
bar <- cacheSolve(foo)

# Print the results of the inverse matrix
print(bar)