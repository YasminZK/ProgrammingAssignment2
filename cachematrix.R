## Put comments here that give an overall description of what your
## functions do
## These functions cache the inverse of a matrix to avoid redundant calculations.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # 'inv' will store the cached inverse
  
  # Setter function: update the matrix and clear cached inverse
  set <- function(y) {
    x <<- y       # assign new matrix to x in parent env
    inv <<- NULL  # reset cached inverse because matrix changed
  }
  
  # Getter function: returns the matrix
  get <- function() x
  
  # Setter for inverse: store the inverse matrix in cache
  setInverse <- function(inverse) inv <<- inverse
  
  # Getter for inverse: return cached inverse matrix
  getInverse <- function() inv
  
  # Return a list of the above functions so they can be called externally
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # try to get cached inverse
  
  # If cached inverse exists, return it with a message
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise, get the matrix data
  data <- x$get()
  
  # Calculate the inverse using solve()
  inv <- solve(data, ...)
  
  # Cache the inverse for future calls
  x$setInverse(inv)
  
  # Return the inverse matrix
  inv
}
## Functions for caching matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  # ... your function code here ...
}

cacheSolve <- function(x, ...) {
  # ... your function code here ...
}

## Testing the caching functions

# Create a matrix
mat <- matrix(c(2, 0, 0, 2), nrow = 2, ncol = 2)

# Create the special cached matrix object
cachedMat <- makeCacheMatrix(mat)

# Calculate inverse first time (should compute and cache)
inv1 <- cacheSolve(cachedMat)
print(inv1)

# Calculate inverse second time (should get cached and print message)
inv2 <- cacheSolve(cachedMat)
print(inv2)

# Change the matrix to a new one
cachedMat$set(matrix(c(1, 2, 3, 4), 2, 2))

# Calculate inverse again (should compute fresh because matrix changed)
inv3 <- cacheSolve(cachedMat)
print(inv3)
