## 
# In this R file, there are two functions: makeCacheMatrix, cacheSolve. They'll be used in this way:
#   > nn <- makeCacheMatrix(mm) ## assume your matrix is 'mm'(invertible)
#   > cacheSolve(nn)   ## calculate and cache the invertible matrix of 'mm'
#


# The function 'makeCacheMatrix' cache a matrix, and a list of 4 functions:
#
#  set - set the value of 'x' which is the cached matrix
#  get - get the cached matrix from 'x'
#  setInverseM - set the cached inverse matrix value ¡®inver_m'
#  getInverseM - get the cached inverse matrix value ¡®inver_m'
#
makeCacheMatrix <- function(x = matrix()) {
    inver_m <- NULL
    set <- function(y) {
        x <<- y
        inver_m <<- NULL
    }
    get <- function() x
    setInverseM <- function(inv_m) inver_m <<- inv_m
    getInverseM <- function() inver_m
    list(set = set, get = get,
         setInverseM = setInverseM,
         getInverseM = getInverseM)
}

#
# The function 'cacheSolve' either 
#       computes the inverse matrix of the cached matrix 'x' if 'inver_m' is NULL, and cache
#       the result to 'inver_m';
# or
#       get the cached inverse matrix 'inver_m'.
# Either way it will return the inverse matrix 'inver_m' of matrix 'x'.
#       
#
cacheSolve <- function(cache_M, ...) {
    inv_m <- cache_M$getInverseM()    # get cached inverse matrix
    if(!is.null(inv_m)) {             # already exists, return it
        message("getting cached data")
        return(inv_m)
    }
    cached_M <- cache_M$get()         # get cached matrix 'x'
    inv_m <- solve(cached_M, ...)     # calculate inverse matrix
    cache_M$setInverseM(inv_m)        # cache it
    inv_m                             # return the inverse matrix
}
