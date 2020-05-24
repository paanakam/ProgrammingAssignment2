## The functions take in a matrix and calculate the inverse of it (assuming it is a square invertible matrix).
## If the inverse of a particular matrix is already calculated, it is picked up from cache (where it was stored), and displayed.
## If the inverse has not been calculated previously, it calculates it, and displays it.

## This nested function takes a matrix as its input and generates a list containing four elements i.e. set, get, setinverse and getinverse. 
## If no previously calculated values of inverse are present it outputs a list without inverse elements.
## If the function was called for a certain matrix before which is different from the current, it erases the previous inverse value (m) and also provides a list without inverse elements.
## If the same matrix is called again the output is a list with certain values.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function takes the output of above as its input. It picks up the getinverse element and checks if it null.
## If it is null, this means that the inverse value is absent. Thus, it calculates it and even sets the same value of m in above function.
## If it isn't null, this means that the inverse was calculated before and stored in cache. Thus, it prints that value and provides the message "getting cached data".

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
