## This file contains two functions, one of which creates a list
## allowing the users to set and get the value of a matrix
## aS well as set and get a cached inverse value.
## 
## The second function checks if an inverse value has been
## cached, and returns this value; if one has not been cached
## it will calculate, store, and then return the inverse


## this function returns a list containing a function to
## 
## 1.  set the value of a matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse
## 4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
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


## this function can be called to get the inverse of the matrix
## set per the makeCacheMatrix function above
## if this value is cached, it will return the cached value
## otherwise it will calculate and store the inverse

cacheSolve <- function(x, ...) {
  ## get the mean value from the cache function
  m <- x$getinverse()

  ## if this value is not null, then we know the inverse has been calculated
  ## go ahead and return this value and exit the function
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## otherwise get the data, calculate the inverse, store value in the cashe
  ## and return this value
  data <- x$get()  
  m <- solve(data, ...)
  x$setinverse(m)
  m  
}
