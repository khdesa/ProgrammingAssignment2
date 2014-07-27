
##This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  #inv will store the cached matrix
  m <- NULL
    
  #sets the value of the matrix
  set <- function(matrix) {
	    
	x <<- matrix
	m <<- NULL
  }
		        
  #gets the value of the matrix
  get <- function() {
	x
  }
				    
  #sets the inverse of the matrix
  setInverse <- function(inverse) {
	m <<- inverse
  }
					        
  #gets the inverse of the matrix
  getInverse <- function() {
	m                                                                         }

  list(set = set, get = get, 
  setInverse = setInverse,
  getInverse = getInverse)
}

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
##should retrieve the inverse from the cache.

##the function assumes that the matrix is always invertible
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
    
  if (!is.null(m)) {
     message("getting cached inverse for the matrix")
     return(m)
  }
		  
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
