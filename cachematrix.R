## Cache the inverse of a matrix

## Create a list object for caching the inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Find the inverse of the matrix using the list
# created above.
## If the list doesn't have a cached inverse,
## find the inverse and cache it.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return (i)
  }
  
  ## This is for when there isn't anything in the cache
  ## Get the inverse and save it in the cache
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}