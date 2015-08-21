##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

## makeCacheMatrix is a function that stores a list of functions.
##set() : changes the vector stored in the main function.
##get() : returns the vector x stored in the main function
##get_inverse(),set_inverse() : set_inverse and get_inverse are functions very similar to set and get. 
##They don't calculate the inverse, they simply store the value of the input 
##in a variable m into the main function makeVector (set_inverse) and return it (get_inverse).

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x 
  set_inverse <- function(inverse) m <<- inverse
  get_inverse <- function() m
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}



## The first thing cachemean does is to verify the value m, stored previously with get_inverse, 
##exists and is not NULL. If it exists in memory, it simply returns a message and the value m, 
##that is supposed to be the inverse, else it calculates value to inverse and stores it into m.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$get_inverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_inverse(m)
  m
}
