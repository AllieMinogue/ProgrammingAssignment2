## The pair of function (makeCacheMatrix & cacheSolve cache the inverse
## of a matrix & retrieve it when needed so as to save calculation time)

## makeCacheMatrix function is a list of functions used to cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve function help to retrieve (get) the inverse of a matrix 
## if its has been cached. If not yet cached, it will calculate the 
## inverse of the matrix & cache the result

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
