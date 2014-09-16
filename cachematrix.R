## Functions that take a matrix x, and calculate the inverse of x, storing the inverse in a cache.
## If the inverse has already been calculated, it looks up the inverse in the cache.

## Initializes the cache, and creates a list of calls supported by this function

makeCacheMatrix <- function(x = matrix()) {#sets default x to an empty matrix
  Inverse <- NULL    #create inverse matrix variable, and set to null
  
  set <- function(y) {
    x <<- y    # assigns x to the value y
    Inverse <<- NULL # assigns inverse to NULL if set is called 
  }
  get <- function() x  # create a call to get matrix x
  
  setInverse <- function(solve) Inverse <<- solve   # creates call to calculate inverse of x
  
  getInverse <- function() Inverse      # creates call to get the inverse of x
  
  list(set = set, get = get,       
       setInverse = setInverse,
       getInverse = getInverse)        # creates list of calls in this function
}



## Function returns a matrix that is the inverse of x, the matrix passed to the function,
## looking up the inverse if already calculated, or calculating the inverse if needed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  Inverse <- x$getInverse()       # gets Inverse of X from Cache
  
  if(!is.null(Inverse)) {         # if inverse for x already cached, get it out of cache
    message("getting cached data")
    return(Inverse)
  }                               # else get matrix and calculate inverse
  
  message("calculating inverse")
  data <- x$get()                 # get the matrix data
  Inverse <- solve(data, ...)     # solve for inverse
  x$setInverse(Inverse)           # set it new value
  Inverse                         # return new value
  
}
