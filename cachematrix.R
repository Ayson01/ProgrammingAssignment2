# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize the inverse property to NULL
  
  # Method to set the matrix and reset the cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Method to get the matrix
  get <- function() x
  
  # Method to set the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  
  # Method to get the inverse of the matrix
  getInverse <- function() inv
  
  # Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
# If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Retrieve the cached inverse, if it exists
  
  if (!is.null(inv)) {
    message("getting cached data")  # Print a message if using cached data
    return(inv)  # Return the cached inverse
  }
  
  data <- x$get()  # Get the matrix from our special "matrix" object
  inv <- solve(data, ...)  # Compute the inverse using the solve function
  x$setInverse(inv)  # Cache the computed inverse
  
  inv  # Return the inverse
}

# Example usage:

# Create a special "matrix" object
my_matrix <- makeCacheMatrix(matrix(c(1, 2, 3, 4), nrow = 2, ncol = 2))

# Compute and cache the inverse
inv_matrix <- cacheSolve(my_matrix)

# Retrieve the cached inverse
inv_matrix_cached <- cacheSolve(my_matrix)  # This will print "getting cached data"
