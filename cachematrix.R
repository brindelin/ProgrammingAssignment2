## makeCacheMatrix accepts a matrix as input and outputs a list
## the list contains definitions of the 'methods' of the matrix
## the methods allow us to get and set values from cache

makeCacheMatrix <- function(x = matrix()) 
{
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## setCache accepts the matrix list as input and returns its inverse
## if the inverse has been calculated it returns it from cache
## if not it calculates the inverse and returns it

cacheSolve <- function(x, ...) 
{
  m <- x$getinverse()
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
