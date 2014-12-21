## this pair of functions are to cache the inverse of a matrix


## following function creates a special "matrix" containing a list of funcitions
makeCacheMatrix <- function(x = matrix()) {
  ## creates a special "matrix" object that can cache its inverse
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set,
       get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


##following function calculate the sovle of special "matrix" with makeCacheMatrix.
##If calculated, then return the solve calculated before with a message "getting cached data"
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  
  
  data <- x$get()
  
  s <- solve(data, ...)
  
  x$setsolve(s)
  
  s
}
