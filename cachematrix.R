## The following functions shall be used to cache a given matrix and calculate
## and cache its inverse

## "makeCacheMatrix" function is used to cache a given matrix
## 
## to set a matrix and cache it, type:
## > x <- makeCacheMatrix()
## > x$setmat(y)
## where y is the matrix to be cached
## with each setting for the matrix, its inverse is set to NULL matrix
##
## to display the cached matrix, type:
## > x$getmat()
##

makeCacheMatrix <- function(x = matrix()) {
        x_inv <- matrix()
        setmat <- function(y) {
                x <<- y
                x_inv <<- matrix()
        }
        getmat <- function() x
        setinv <- function(inv) x_inv <<- inv
        getinv <- function() x_inv
        list(setmat=setmat, getmat=getmat, setinv=setinv, getinv=getinv)
}


## "cacheSolve" function return a matrix that is the inverse of matrix 'x'
## if the result is already cached (x_inv is not NULL), no need to calculate it
## just display the value of x_inv, if x_inv is NULL, then it should be calculated
## using solve() function, then cached using the above described function setinv()

cacheSolve <- function(x, ...) {
        x_inv <- x$getinv()
        if (!all(is.na(x_inv))) {
                message("get the result from cache")
                return(x_inv)
        }
        my_mat <- x$getmat()
        my_inv <- solve(my_mat)
        x$setinv(my_inv)
        my_inv
}
