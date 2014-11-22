## These functions have to create a funcionallity that let you computed the inverse matrix from
## one send as a parameter and cache it instead compute it each time, in order to save memory resources.

## Function for store the inverse matrix from the original one.
makeCacheMatrix <- function(originalMat = matrix()) {
    inverseMat <- NULL
    set <- function(y) {
        originalMat <<- y
        inverseMat <<- NULL
    }
    get <- function() originalMat
    setInv <- function(invMat) inverseMat <<- invMat
    getInv <- function() inverseMat
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Function for compute or get the inverse matrix stored by above makeCacheMatrix function.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInv()
    if(!is.null(m)) {
        message("Getting cached inverse matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInv(m)
    m
}
