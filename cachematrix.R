## The first function, makeVector creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse  

makeCacheMatrix <- function(x = matrix()) {
	matrixInv<-NULL
    set <-function(y){
      x<<-y
      matrixInv<<-NULL
    }
    
  ## The following function calculates the inverse of the matrix
  ## created with the above function. However, it first checks 
  ## to see if the inverse has already been calculated. If so, it   
  ## gets 
  ## the inverse from the cache and skips the computation. 
  ## Otherwise, 
  ## it calculates the inverse of the data and sets 
  ## the value of the inverse in the cache via the setinverse  
  ## function.
  
  get<-function()x
  setinverse<-function(inverse) matrixInv <<- inverse
  getinverse<-function()inv
  list(set=set,get=get,setinverse,getinverse=getinverse)

}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been  
## calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrixInv <- x$getinverse()
  if(!is.null(matrixInv)) {
    message("getting cached data")
    return(matrixInv)
  }
  data <- x$get()
## solve function: computes 
## the inverse of a square matrix  
  matrixInv <- solve(data, ...) 
  x$setinverse(matrixInv)
  matrixInv
}
