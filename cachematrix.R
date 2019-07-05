## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##This function makes a cache matrix to be leveraged in the caching the inverse. 
makeCacheMatrix <- function(x = matrix()) {
  invs<-NULL
  set<-function(y){                               ##set the matrix
    x<<-y
    invs<<-NULL
  }
  get<-function() x                               ##get the matrix 
  setInverse<-function(inverse) invs<<- inverse   ##set the inverse of the matrix 
  getInverse<-function() invs                     ##get the inverse of the matrix 
  
  list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse)
}

## Write a short comment describing this function

##This function will compute the inverse of a matrix and cache the result, if this function is used 
## again on the same matrix the previously computed inverse that was stored will be returned, saving 
## extra computation time.
cacheSolve <- function(x, ...) {
  invs<-x$getInverse()
  if(!is.null(invs)){
    return(invs)
  }
  matrix<-x$get()
  invs<-solve(matrix, ...)
  x$setInverse(invs)
  invs
}