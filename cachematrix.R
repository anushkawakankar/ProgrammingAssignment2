## Put comments here that give an overall description of what your
## functions do

## this function creates a list of functions, which are the foll
## 1) set - sets the value of matrix
## 2) get - gets the value of the matrix
## 3) setInverse - sets the value of inverse
## 4) getInverse - gets the value of inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m<<- inverse
  getInverse <- function() m
  
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function first checks if the inverse has already been calculated. If it is calculated already, it simply gets it from
## cache and prevents further computation. Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the
## setInverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m))
  {
    print("getting data")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat,...)
  x$setInverse(m)
  m
}
