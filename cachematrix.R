## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Function to generate list of functions to store a matrix and its 
# inverse in cache and retrive them
makeCacheMatrix <- function(x = matrix()) {
  
  invMatrix <- NULL
  
  # Function to store matrix in cache 
  # When new matrix is stored, inverse matrix of the old matrix is deleted
  set <- function(y) {
    
    x <<- y
    invMatrix <<- NULL
    
  }
  
  # Function to retrieve matrix stored in cache
  get <- function() x
  
  # Function to store inverse matrix in cache 
  setInverse <- function(inverse) invMatrix <<- inverse
  
  # Function to retrieving inverse matrix from cache
  getInverse <- function() invMatrix
  
  # Returning the list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function

# Function to return inverse matrix of a matrix stored in cache, either by 
# computing the inverse or retrieving it from cache
# Takes vector returned by makeCacheMatrix as input
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # Retrieving inverse matrix stored in cache
  invMatrix <- x$getInverse()
  
  # If inverse matrix is stored in cache, returning the same
  if(!is.null(invMatrix)) {
    print("getting cached data")
    return(invMatrix)
  }
  
  # If inverse matrix is not stored, retrieving the matrix from cache
  matrix <- x$get()
  
  # Computing the inverse of matrix and storing the inverse in cache
  invMatrix <- solve(matrix, ...)
  x$setInverse(invMatrix)
  
  # Returning the inverse matrix
  invMatrix
}
