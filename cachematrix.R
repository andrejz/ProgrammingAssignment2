## A pair of functions makeCacheMatrix and CacheSolve is able to cache an
## inverse of a matrix. If a cached inverse of a matrix does not exist yet,
## it will be evaluated and stored into cache for next time.

## makeCacheMatrix creates a special matrix object which can cache its
## inverse 

makeCacheMatrix <- function(mat = matrix()) {

  ## Initializes the inverse value
  inv <- NULL
  
  
  ## Sets the value of the of the matrix
  set <- function(y) {
    ## Sets the value of the matrix
    mat <<- y
    ## Set the inverse to null
    inv <<- NULL
  }
  
  
  ## Returns the value of the matrix
  get <- function(){
    mat
  } 
  
  
  ## Sets the inverse of the matrix
  setinv <- function(solve) {
    inv <<- solve
  }
  
  
  ## Obtains the inverse value of the matrix
  getinv <- function() {
    inv
  }
  
  ## Returns a list of methods so they can be called by
  ## other functions
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Calculates the inverse of a special matrix object created by
## the function makeCacheMatrix. When an inverse is calculated
## it is stored into a cache. If an inverse has already been
## calculated and is stored in cache an inverse value will be
## obtained from the cache
cacheSolve <- function(x, ...) {
  ## Obtains an inverse value of x from cache
  inv <- x$getinv()
  
  ## If a cached value exists, return it 
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## Otherwise get the matrix information from the object
  data <- x$get()
  
  ## Calculate the inverse of a matrix
  inv <- solve(data, ...)
  
  ## Stores the inverste to the cache for future use 
  x$setinv(inv)
  
  ## Returns the inverted matrix value
  inv
}