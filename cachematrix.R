# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  x_inverse <- NULL # set inverse to NULL
  set <- function(y){ # Initialize matrices
    x <<- y # set matrix
    x_inverse <<- NULL # reset inverse to NULL
  }
  get <- function() x # get initial matrix
  set_inverse <- function(inv) x_inverse <<- inv # set inverse matrix
  get_inverse <- function() x_inverse # get inverse matrix
  
  # return list
  list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
# inverse has already been calculated (and the matrix has not changed) ==>cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$get_inverse()
  ## if inverse exists ==> get from cache
  if (!is.null(inv)){ 
    message("getting cached data")
    return(inv)
  }
  ## else: get matrix + calculate inverse
  data <- x$get()
  inv <- solve(data) # calc inverse
  x$set_inverse(inv) # set inverse
  inv
}
