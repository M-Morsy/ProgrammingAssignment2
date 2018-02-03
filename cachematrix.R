## Put comments here that give an overall description of what your
## functions do


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  # initialize inverse value
  I <- NULL 
  # I <- solve (x) , this is in the cache function 
  # set the matrix value if not initialized
  set <- function (y) {
    x <<- y
    I <<- NULL
    }
  # get the matrix value
  get <- function () x
  # set the inverse value 
  setInv <- function (solve) I <<- solve
  # get the inverse value 
  getInv <- function () I
  list (set = set , get = get ,
        setInv = setInv , 
        getInv = getInv)
}


##Return a matrix that is the inverse of 'x'
# Note that x here is not the matrix itself, but it's the list containing all the function needed to get inverse of the matrix
cacheSolve <- function(x, ...) {
        # Check if the inverse already exists
        I <- x$getIn()
        # print (I)
        if (!is.null(I)){
          return (I)
        }
        data <- x$get()
        # print (data)
        I <- solve (data, ...)
        x$setInv (I)
        # Return the inverse of the matrix 
        I
}


