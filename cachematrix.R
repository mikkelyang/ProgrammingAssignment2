## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function take in a matrix with the capacity to store its inverse in its cache using the other function below.

makeCacheMatrix <- function(x = matrix()) {
    mat <- x
    inv <- NA
    get <- function () mat
    set_inverse <- function (inverse) inv <<- inverse
    get_inverse <- function () inv
    list(get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}


## Write a short comment describing this function
# This function tries to retrieve the cached inverse of a matrix. If this is still not computed,
# it goes ahead and computes for the inverse and stores it in cache so that the next time the
# inverse is needed, it will not be computed anymore but can be retrieved from cache.
# Allowing for faster computing.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$get_inverse()
    if(!is.na(inv)){
            print('Getting from Cache')
            return(inv)
    }
    data <- x$get()
    comp_inv <- solve(data)
    x$set_inverse(comp_inv)
    comp_inv
}
