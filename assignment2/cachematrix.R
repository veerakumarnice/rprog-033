## these funtions are used to avoid calculating inverse again 
## one it is calculated.

## returns a list of cache vetors to the object for matrix inverse calculation
makeCacheMatrix <- function(x = matrix()) {
  ## sets the inverse matrix to be null at beginning
  m <- NULL
  set <- function(input) { ## sets a new matrix so inverse is set to null
    x <<- input
    m <<- NULL
  }
  ## returns the matrix
  get <- function() x 
  ## sets the inverse cache
  setsolve <- function(input) m <<- input
  ## gets the inverse cche
  getsolve <- function() m
  ## returns a list of the cache vectors at the creation of the object
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## chechks whether inverse is already calculated if not 
##calculates a new inverse
## Returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        
  ##the getsolve methods returns the value precomputed inverse if any
  temp <- x$getsolve()
  ##if the inverse is already computed the existing value is returned
  if(!is.null(temp)) {
    ## displays a message that it is precomputed
    message("getting cached data")
    return(temp)
  }
  ## if the inverse has not yet calculated then the inverse is calculated
  ## and stored in the cache
  data <- x$get()
  solved <- solve(data)
  x$setsolve(solved)
  ## the computed inverse is returned
  solved
}
