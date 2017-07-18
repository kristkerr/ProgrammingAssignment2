##This function makeCacheMatrix has a matrix as an input, sets the value of the matrix,
##gets the value of the matric, sets the inverse of the matrix and gets the inverse matrix.

##<<- operator assigns value to an object in a different environment from its current environment

#take the matrix as an input
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  #set the value of the matrix
  set <- function(y) {
    x<<-y
    i<<- NULL
  }
  
  get<- function() x                              #get the value of the matrix
  setinverse <- function(inverse) i<<-inverse     #set the value of the invertible matrix
  getinverse <- function() i                      #get the value of the invertible matrix
  ##create list with methods for get/set of both original matrix 
  ##and its inverse and return to list of parent environment. 
  ##this allows use of $ operator to access each function from list
  list(set = set, get = get, 
       setinverse = setinverse,  
       getinverse = getinverse)
}


## This function computes the inverse of the "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated and the matrix has not changed, 
##the cacheSolve will get the inverse from the cache

cacheSolve <- function(x, ...) {
  ## get the value of the invertible matrix from makeCacheMatrix function.
  i<- x$getinverse()
  if(!is.null(i)) {                             #if inverse is not NULL
    message("Getting Cached Matrix")            #Type message: Getting Cached Matrix
    return(i)                                   #return invertible matrix
  }
  data<- x$get()                                #get the original matrix data
  i <- solve(data, ...)                         #use solve() to inverse the matrix
  x$setinverse(i)                               #set the invertible matrix
  i                                             #return a matrix that is the inverse of x.
}
