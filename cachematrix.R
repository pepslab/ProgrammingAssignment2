# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1 set the value of the matrix
# 2 get the value of the matrix
# 3 set the value of inverse of the matrix
# 4 get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL        # the result of inversion is stored here
  set <- function(y){
  x <<- y
  minv <<- NULL       # minv initialized to null 
}

get <- function() x   # return the input matrix
setmatrix <- function(solve) minv <<- solve  # set the inversed matrix
getmatrix <- function() minv  # return the inversed matrix
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)

}

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x=matrix(), ...) {
    minv <- x$getmatrix()   # gets the inversed matrix from x
    if(!is.null(minv)){   # if there is an inversion result
      message("getting cached data")
      return(minv)    # returns the calculated inversion
    }
    matrix <- x$get()      # if not we get the matrix object
    minv <- solve(matrix, ...)  #solves it
    x$setmatrix(minv)      # sets it to the object
    minv    # returns result
}