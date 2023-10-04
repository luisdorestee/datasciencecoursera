
# Create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y                     # Use <<- to assign to x in the parent environment
    inverse <<- NULL            # Reset the cached inverse when the matrix changes
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the inverse of the matrix
  setInverse <- function(solveMatrix) {
    inverse <<- solveMatrix    # Cache the inverse
  }
  
  # Function to get the cached inverse (if available)
  getInverse <- function()  
  
  # Return a list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
############################################################



# Compute and cache the inverse of a matrix if available
cacheSolve <- function(x, ...) {
  # Check if the cached inverse is available
  cachedInverse <- x$getInverse()
  if (!is.null(cachedInverse)) {
    message("Getting cached inverse")
    return(cachedInverse)  # Return the cached inverse if available
  }
  
  # If the cached inverse is not available it computes it:
  data <- x$get()
  inverse <- solve(data, ...)
  
  # Cache the computed inverse
  x$setInverse(inverse)
  
  # Return the computed inverse
  inverse
}



############################################################
# Create a cacheable matrix
mat <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2))
print(matrix(c(1, 2, 3, 4), nrow = 2))

# Compute and cache the inverse
cacheSolve(mat)
