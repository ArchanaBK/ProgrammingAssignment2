## makeCacheMatrix takes an argument of type matrix(). within makeCacheMatrix,
#initialize i to NULL. x and i are within makeCacheMatrix environment. 
#set function takes an argument y matrix. y is assigned to x. i is set to NULL
# to clear any older value in the inverse in case of a change to the input matrix.

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
}
#get function returns the value of x
    get <- function()x
#setinverse function sets the i (in makevector_matrix environment with the argument
# passed to it.
  setinverse <- function (inversed) i <<- inversed
#getinverse returns the i (from makevector_matrix environment)
  getinverse <- function ()i
#return a list.
  list (set = set, get = get, 
        setinverse = setinverse, 
        getinverse = getinverse)
}

## cacheSolve takes a vector of type makevector_matrix. get inverse check if
#inverse is null, if yes, find the transpose. 
#if inverse was not set to NULL, it means the matrix has not changed...
#print the message and return inverse i from makeCacheMatrix environment

cacheSolve <- function(x, ...) {
  i<- x$getinverse()
  if(!is.null(i)){
    message("Getting cache inverse")
    return(i)
  }
  data <-x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
