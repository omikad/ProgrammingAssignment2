# This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # m holds cached value
  m <- NULL
  
  # set function changes the matrix, and clears cache
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # get the matrix
  get <- function() x
  
  # set cache with the provided value
  setinverse <- function(inverse) m <<- inverse
  
  # get cached value
  getinverse <- function() m
  
  # return these functions as a list
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated (and
# the matrix has not changed), then the cachesolve retrieves the 
# inverse from the cache.
cacheSolve <- function(x) {
  # get cached value
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # cached value is null, we need to calculate and save the inverse
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  
  # return a matrix that is the inverse of 'x'
  m
}
