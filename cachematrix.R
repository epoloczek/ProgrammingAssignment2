
makeCacachecheMatrix <- function(m = numeric()) {
         inv <- NULL

         set <- function(y) {
                 m <<- y
                 inv <<- NULL
         }
         get <- function() m
         setinv <- function(x) inv <<- x
         getinv <- function() inv
         list(set = set, get = get,
              setinv = setinv,
              getinv = getinv)
 }


cacheSolve <- function(mList, ...) {
        inv <- mList$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- mList$get()
        inv <- solve(data, ...)
        mList$setinv(inv)
        inv
}