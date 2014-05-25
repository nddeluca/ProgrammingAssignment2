## Set of functions for storing a matrix with it's inverse.

## Returns a list of functions to store and retrieve a
## matrix and it's associtated inverse.
makeCacheMatrix <- function(stored_matrix = matrix()) {

  cached_inverse <- NULL

  set <- function(new_or_updated_matrix) {
    stored_matrix <<- new_or_updated_matrix
    cached_inverse <<- NULL
  }

  get <- function() stored_matrix

  setInverse <- function(new_inverse) cached_inverse <<- new_inverse
  getInverse <- function() cached_inverse

  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Acts upon a cached matrix to return it's inverse.  If the
## inverse has already been computed, then the cached inverse
## of the matrix is returned.  If the inverse has not been
## computed, then it is computed, stored within the
## cached matrix, and returned.

cacheSolve <- function(cached_matrix, ...) {

  cached_inverse <- cached_matrix$getInverse()

  if(!is.null(cached_inverse)) {
    message("getting cached data")
    return(cached_inverse)
  }

  data <- cached_matrix$get()
  new_inverse <- solve(data, ...)
  cached_matrix$setInverse(new_inverse)
  new_inverse
}
