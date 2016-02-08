## Cache the inverse of a matrix

## Do the caching
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
  }
  get <- function() x
  
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Get the stuff from the cache
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