## The functions here defined are intended to create a "special" matrix, which
## can cache its inverse, and to calculate the inverse of this matrix, checking
## if a cached result is available in the first place.

## This function is used to construct the "special" matrix mentioned above.
## Parameters: x -> A numeric matrix. Default is a 1x1 matrix filled with 'NA'
## Returns: A "special" matrix with the following "methods":
##          set -> Takes a numeric matrix as a parameter and sets it as the
##                 "special" matrix's value
##          get -> Returns the value of the "special" matrix
##          setInverse -> Sets the value of the inverse of the matrix.
##                        Takes a numeric matrix as a parameter
##          getInverse -> Returns the inverse of the matrix.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    ## Setting a new value for the matrix will make any previously calculated
    ## inverse to be invalid.
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function is used to obtain the inverse of the "special" matrix.
## It either calculates its inverse or returns a previously calculated
## and cached inverse
## Parameters: x -> A "special" matrix to have its inverse calculated
##             ... -> The user can pass further parameters here to be
##                    redirected to the "solve" function used to
##                    calculate the inverse
## Returns: The inverse of the "special" matrix
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    ##The inverse has been previously calculated and cached. Return this value
    message("getting cached data")
    return(inv)
  }
  ## The inverse has not been calculated yet. Calculate it and store it in the
  ## "special" matrix
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  
  ##Returning the inverse
  inv
}