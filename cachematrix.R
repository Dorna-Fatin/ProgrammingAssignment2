## INTRODUCING a special matix that caches the inverse
## CREATING special matrix Bruh using Bruh <- makeCacheMatrix(x)
## X is a normal Matrix
## (Get the value of X using m$get())
## (Change the value of x using m$set(y), y is a normal matrix)
## GETTING the inverse using cacheSolve(Bruh)

## This function takes the special matrix returned by makeCacheMatrix and
##computes an inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  Bruh <- NULL 
  ## Define the function to SET the value of the matrix
  ## clear any previous inverse matricies 
  set <- function(y) {
    x <<- y
    Bruh <<- NULL
    ## SET the value 
    ## clearing the cache
  }
  ## Defining the function to GET the value of the matrix
  get <- function () x 
  ## Defining the function to SET the inverse of the matrix
  setinverse <- function (inverse) Bruh <<- inverse 
  ## Defining the function to GET the inverse of the matrix
  getinverse <- function () Bruh ## This part 'gets' the value of the inverse of
  ##of the matrix
  
  ## Returning a list
  list (set = set,
        get = get, 
        setinverse = setinverse,
        getinverse = getinverse)
}

## COMPUTES the inverse of the special matrix 
## IF the inverse has already been calculates
## AND the matrix has not changed
## THEN cacheSolve RETRIEVES the inverse

cacheSolve <- function(x, ...) {
  Bruh <- x$getinverse() 
  ## retrieving cached value of the inverse of the matrix
  if (!is.null(Bruh)) { 
        message ("getting cached data")
        return (Bruh)
        ## If the cache is not empty, return the value
  }
  ## If we make it this far, the cache must be empty so it needs to be
  ## calculated, cached and then returned
  data <- x$get() 
  ## GETTING the value of the matrix (original)
  Bruh <- solve(data, ...) 
  ## CALCULATING the inverse of the matrix
  x$setinverse (Bruh) 
  ## CACHEING (is that a word?) what was calculated 
  Bruh 
  ##RETURINING what was cached 
}
