##Following functions help not to solve inverse of matrix every time there is need to use it, but only once.
##(So running it first time takes the same as solving it in a "standard" way, every next time we spare time)


##Function that return list of functions. In function getinv inverse matrix is being kept, so there is no need to solve it again and again (e.g. in case of copying)

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

## Function that check if the inverse matrix has been already solved. If it has been than function returns existing solution, in other case function solves it. 

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