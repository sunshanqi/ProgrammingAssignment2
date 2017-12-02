## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  matrixInverseCopy<-NULL      # Initialized matrixInverseCopy with NULL
  set<-function(y){
    x <<- y                     # Assign value of matrix i.e. 'x'
    matrixInverseCopy <<- NULL         
  }
  get <- function() x        # get value of matrix i.e. 'x'
  
  # setInverse function calcualtes inverse of matrix 'x' and assign the 
  # result to matrixInverseCopy
  setInverse <- function (solve) matrixInverseCopy <<- solve
  # getInverse function is used to get inverse of matrix 'x'
  getInverse <- function() matrixInverseCopy
  
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrixInverse <- x$getInverse()  # Retrieve matrix inverse 
  if(!is.null(matrixInverse)){   # If matrix inverse is calculated
    message("Getting cached data: Matrix inverse is already calculated")
    return(matrixInverse) # Return a matrix that is the inverse of 'x'
  }
  data <- x$get()      #  Retrieve a matrix using get method
  matrixInverse <- solve(data, ...)  # Calculate inverse of matrix
  x$setInverse(matrixInverse) # Set value of inverse of matrix
  (matrixInverse)  # Return a matrix that is the inverse of 'x'
}
