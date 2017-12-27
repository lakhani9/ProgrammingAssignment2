#================================================================================================
#  Assign. 2 - makeCacheMatrix 
#  This function creates a special "matrix" object that can cache its inverse. It has 
#  4 functions (sections)
#              
#              1) set value of the matrix
#              2) get value of the matrix
#              3) setInv value of the inverse
#              4) getInv value of the inverse
# 
# Parameter:   mat    - Matrix to cache (invert)
# return:      return - the special matrix 
# 
#===============================================================================================

makeCacheMatrix <- function(mat = matrix()) {
  
  inverse <- NULL #  inverse to null
  
  # set the value of the matrix
  set <- function(y) {  
    mat <<- y
    inverse <<- NULL
  }
  
  # return value of the matrix
  get <- function() {
    mat
  }
  
  #set(cache) value of the inverse of the matrix
  setInv <- function(cache) {
    inverse <<- cache
  }
  
  #return value of the inverse of the matrix
  getInv <- function() {
    inverse 
  } 
  
  #return the special matrix
  list(set = set, get = get, setInv = setInv, getInv = getInv)
  
}

# =================================================================================================
#  Assign 2 - cacheSolve 
#   This function computes inverse of the special "matrix" returned by 
#    makeCacheMatrix above. If the inverse has already been calculated (and the matrix 
#    has not changed), then cachesolve should retrieve the inverse from the cache.
#              
# Parameter:   mat - matrix
# return:      the cached matrix (inversed) 
#
# 
# =================================================================================================


cacheSolve <- function(mat, ...) {
  
  #  First check to see if the inverse has already been calculated.
  inverse <- mat$getInv()
  
  # if inverse has already been calculated and cached, 
  # return this instance
  if (!is.null(inverse)) {
    
    message("getting cached matrix")
    return(inverse)
  }
  
  # if not cached, calculate it using solve() and cache the matrix, then return it
  inverse <- solve(mat$get(), ...)
  mat$setInv(inverse)
  inverse
}

