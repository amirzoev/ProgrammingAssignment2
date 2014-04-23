## Put comments here that give an overall description of what your
## functions do

## The function creates special matrix object, and defines interface functions
## to get/set the value of direct matix and get/set values of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL # variable keeping the inverse matrix
  set <- function(y) { # function setting the matrix
    x <<- y 
    i <<- NULL
  }
  get <- function() x # function returning the matrix
  setinv <- function(inv) i <<- inv # function storing the inverse matrix
  getinv <- function() i # function returning the inverse matrix
  list(set = set, get = get, 
       setinv = setinv,
       getinv = getinv) # return list of the functions
}




## Function checks if an inverse matix for the given matrix has being already calculated. 
## If not, the inverse matrix is calculated and stored, othervise the pre-calculated 
## matrix is returend

cacheSolve <- function(x, ...) {
  i <- x$getinv() # Get the stored inverse matrix 
  if(!is.null(i)) { # Stored inverse matrix exists
    message("getting cached data")
    return(i) # return the inverse matrix and leave the function
  }
  #If execution came to this point, it means that the inverse function does not exists, so it has to be calculated
  data <- x$get() # get value of the original matrix
  i <- solve(data, ...) # calculate inverse  matrix
        ## Return a matrix that is the inverse of 'x'
  x$setinv(i) # Store the inverse matrix
  i # return the inverse matrix
}
