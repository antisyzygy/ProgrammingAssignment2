## These functions are used for computing a matrix inverse
## with the added ability to cache the matrix inverse if it
## was previously computed

## creates a cached matrix function type, with utility methods
## for storing a copy of it's own matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  minv <- NULL;
  
  set <- function(y) {
    x <<- y;
    minv <<- NULL;
  }
  
  get <- function() {
    return(x);
  }
  
  setInverse <- function(solve) {
    minv <<- solve;
  }
  
  getInverse <- function() {
    return(minv);
  }
  
  return(list(set = set, 
              get = get,
              setInverse = setInverse, 
              getInverse = getInverse));
}

## Accepts input of a cached matrix function (see above)
## returns the matrix inverse, either from cache or newly computed

cacheSolve <- function(x, ...) {
  minv <- x$getInverse();
  
  if(!is.null(minv)) { 
    message("Returned cached inverse.");
    return(minv);
  }
  
  data <- x$get();
  minv <- solve(data, ...);
  
  x$setInverse(minv);
  
  message("Returned newly computed inverse.");
  return(minv);
}