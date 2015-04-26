## Using these two functions you can find the inverse of a matrix
## and if you have previously calculated the inverse of the matrix
## then you do not need to recalculate it as it will be cached.

## The makeCacheMatrix function creates a special matrix which is 
## really a list containing the functions needed to set the matrix,
## get the matrix, set the inverse of the matrix and get the inverse
## of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  set <- function(new_matrix) {
    x <<- new_matrix
    inv_matrix <<- NULL
  }
  get <- function() x
  set_im <- function(new_inv_matrix) inv_matrix <<- new_inv_matrix
  get_im <- function() inv_matrix 
  list(set = set, 
       get = get,
       set_im = set_im,
       get_im = get_im)
}


## The cacheSolve function calculates the inverse of the special 
## matrix created with the makeCacheMatrix function. It calls the 
## get inverse matrix function on the special matrix first and checks 
## if it has been calculated already (not equal to NULL). If it has 
## already been calculated it prints the message "getting cached data" 
## and returns the inverse of the matrix. Otherwise, it calculates 
## the inverse of the matrix itself and prints that.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv_matrix <- x$get_im()
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  data <- x$get()
  inv_matrix <- solve(data, ...)
  x$set_im(inv_matrix)
  inv_matrix
}