## OVERALL, THESE TWO FUNCTIONS FIRST CREATES
## A LIST TO GET AND STORE A MATRIX, AND GET AND
## STORE THE INVESRSE OF A MATRIX.  THE SECOND FUNCTION
## WILL RETURN THE INVERSE OF A MATRIX, EITHER RETRIEVING
## IT FROM THE CACHE, OR CALCULATING THE INVERSE

## THIS FUNCTION CREATES A LIST OF THE FUNCTIONS
## TO GET AND STORE THE INVERSE OF A MATRIX
## WITHIN A CHILD ENVIRONMENT

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y) {
      x <<- y
      inv <- NULL
   }
   get <- function() x
   setinv <- function(myinv) inv <<- myinv
   getinv <- function() inv
   list(set = set, get=get,
        setinv = setinv,
        getinv = getinv
      )
}

## THIS FUNCTION WILL COMPUTE THE INVERSE
## BY EITHER RETRIEVING IT FRM THE CACHE
## IF IT ALREADY EXISTS, OR CALCULATING IT

cacheSolve <- function(x, ...) {
   inv <- x$getinv()
   if(!is.null(inv)) {
      message("getting cached inverse")
      return(inv)
   }
   data <- x$get()
   inv <- solve(x)
   x$setinv(inv)
   inv
}
