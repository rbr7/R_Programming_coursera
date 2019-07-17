## Put comments here that give an overall description of what your
## functions do

# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  # Define function to set the value of the matrix. It also clears the old
  # inverse from the cache
  
  set <- function(y){
    x <<- y          # set the value of the matrix
    inv <<- NULL     # Clear the cache
  }
  
  # Define function to get the value of the matrix
  get <- function() x
  
  # Define function to set the inverse. This is only used by getinverse() when
  # there is no cached inverse
  setinverse <- function(inverse) inv <<- inverse
  
  # Define function to get the inverse
  getinverse <- function() inv
  
  # Return a list with the above four functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# Return inverse of matrix x
# 
# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve retrieves the 
# inverse from the cache.

cacheSolve <- function(x, ...) {
  # To Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()  # to fetch the cached value for the inverse
  
  if(!is.null(inv)){     # returns cache if its not null
    message("Getting cached data !!")
    return(inv)
  }
  
  # If cache is empty, need to calculate inverse, cache it and return
  data <- x$get()          # to get the value of matrix
  
  inv <- solve(data, ...)  # computing the inverse
  
  x$setinverse(inv)        # caching the inverse
  
  inv                      # result returned
}


# Example Run

# r <- rbind(c(2, -1/4), c(-1/4, 2))
#> m <- makeCacheMatrix(r)
#> m$get()
#       [,1]  [,2]
# [1,]  2.00 -0.25
# [2,] -0.25  2.00

# since computing first time, so no cache

# > cacheSolve(m)
#         [,1]       [,2]
# [1,] 0.50793651 0.06349206
# [2,] 0.06349206 0.50793651

# trying again and getting cache data.....

# Getting cached data !!
#         [,1]       [,2]
# [1,] 0.50793651 0.06349206
# [2,] 0.06349206 0.50793651

