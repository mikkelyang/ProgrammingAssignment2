## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    mat <- x
    inv <- NA
    get <- function () mat
    set_inverse <- function (inverse) inv <<- inverse
    get_inverse <- function () inv
    list(get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}


## Write a short comment describing this function

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
