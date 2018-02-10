## Put comments here that give an overall description of what your
## functions do


## Write a short comment describing this function

#makecachematrix fucntion is created to build a new matrix which can cache its inverse.
# This function takes an input value to build a matrix.
# It will set the matrix to that initial value, get the value as well.
# It will set the Inverse matrix and gets it as well.
# This will enable the matrix object to cache its own inverse matrix.

makeCacheMatrix <- function(x = matrix()) {                             #Creating the function
  inverse_matrix <- NULL                                                # Setting it to NULL 
  set <- function(y) {                                                  #Setting the value of the matrix
    x <<- y
    inverse_matrix <<- NULL 
  }
  get <- function() x                                                    #Getting the value of the matrix
  setinverse <- function(inverse) inverse_matrix <<- inverse             #Setting the Inverse of the matrix
  getinverse <- function() inverse_matrix                                #Getting the Inverse of the matrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
# Cachesolve function pulls the output of the makeCacheMatrix and uses it as the input and checks if there is any value for Inverse matrix.
# If it finds a value for it it will skip the computation but if it is empty the calculates the inverse and sets the value in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse_matrix <- x$getinverse()
  
  if (!is.null(inverse_matrix)) {                         #It will get the input from the cache
    message("Getting cached inverse matrix")
    return(inverse_matrix)
  }
  inversedata <- x$get()
  inverse_matrix <- solve(inversedata, ...)
  
  x$setinverse(inverse_matrix)                             #It will set the value of the inverse matrix in cache
  return(inverse_matrix)
}


#OUTPUT: 
# > matrix2 <- matrix(1:4, 2, 2)
# > matrix2
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > matrix_cache <- makeCacheMatrix(matrix2)
# > matrix_cache$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > matrix_cache$getinverse()
# NULL
# > cacheSolve(matrix_cache)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(matrix_cache)
# Getting cached inverse matrix
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# >
