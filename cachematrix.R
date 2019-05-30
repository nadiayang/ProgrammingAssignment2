## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Caching inverse of a matrix:
## Below are two functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
    
  }
  get <- function() x
  setinv<-function(inverse)inv<<-inverse
  getinv<-function()inv
  list(set=set,get=get,
       setinv=setinv,
       getinv=getinv)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null (inv))  {
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinv(inv)
  inv
}

##test
matrix1<-makeCacheMatrix(matrix(1:4,2,2))
matrix1$get()
cacheSolve(matrix1)
matrix1$set(matrix(c(2, 2, 1, 4), 2, 2))
matrix1$get()
cacheSolve(matrix1)
