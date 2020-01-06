makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
        set <- function(y) {
              x <<- y
              m <<- NULL
       }
       get <- function() x
       setinversematrix <- function(solve) m <<- solve
       getinversematrix <- function() m
       list(set = set, get = get,
            setinversematrix = setinversematrix,
            getinversematrix = getinversematrix)
}

cacheSolve<- function(x, ...) {
       m <-x$getinversematrix()
       if(!is.null(m)) {
              message("getting cached data")
              return(m)
       }
       data <- x$get()
       m <- solve(data, ...)
       x$setinversematrix(m)
       m
}
mat<-matrix(c(1,2,3,4),2,2)
m1<-makeCacheMatrix(mat)
cacheSolve(m1)
