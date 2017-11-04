## Put comments here that give an overall description of what your
## functions do

##1. First function will cache its own inverse
##2. Second function will call the first looking for a cached inverse

## Write a short comment describing this function
##This function will cache its own inverse

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL       ##Create an inverseMatrix object as NULL (will hold the inverse)
  setMatrix <- function(y) {  ##setMatrix function will receive y and set that to x 
                              ##with the <<- operator because the other object is
                              ##in the parent environment
    x <<- y
    inverseMatrix <<- NULL    ##If there's a new matrix, set it to NULL
  }
  getMatrix <- function() x   ##get function, returns x
  setInverse <- function(inverse) inverseMatrix <<- inverse   ##set the inverse function, sets the inverse
  getInverse <- function() inverseMatrix                      ##get the inverse function, returns inverseMatrix
  list(setMatrix = setMatrix, 
       getMatrix = getMatrix,  
       setInverse = setInverse,
       getInverse = getInverse)         ##make it possible to use the functions from outside with $                              
}


## Write a short comment describing this function
## This function will return a matrix that is the inverse of x
cacheSolve <- function(x, ...) {
  
  invertedMatrix <- x$getInverse()                #get the value of the inverted matrix from the previous function
  if(!is.null(invertedMatrix)) {                  #check if we have a cached inverted matrix
    message("Getting cached inverted matrix")
    return(invertedMatrix)                        #return inverted matrix
  }
                                            #else, if we don't have it cached 
  matrix <- x$getMatrix()                   #getMatrix
  invertedMatrix <- solve(matrix, ...)      #inverse the matrix, ... passed from other methods
  x$setInverse(invertedMatrix)              #set invertedMatrix 
  return(invertedMatrix)                    #return invertedMatrix
}

##Test

testMatrix <- matrix(1:4,1:4)
cacheMatrix <- makeCacheMatrix(testMatrix)
cacheMatrix$getMatrix()
cacheMatrix$getInverse()
cacheSolve(CacheMatrix)
