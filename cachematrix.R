# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # Following the same format as the assignment example

    # Get the current state of the inverse and see if it
    # has been computed yet
    inv <- x$getinverse()

    # If it has...
    if(!is.null(inv)) {
    	# Simply return the computed inverse		
        message("Getting cached matrix")
        return(inv)
    }

    # If it hasn't...
    # Get the matrix itself
    data <- x$get()

    # Find the inverse
    inv <- solve(data, ...)

    # Cache this result in the object
    x$setinverse(inv)

    # Return this new result
    inv    
}



 ## Sample run:
 ##>  x = rbind(c(1, -1/8), c(-1/8, 1))
 ##> m = makeCacheMatrix(x)
 ##> m$get()
 ##       [,1]   [,2]
 ##[1,]  1.000 -0.125
 ##[2,] -0.125  1.000
 ##> 
 ##> 
 ##>  cacheSolve(m)
 ##          [,1]      [,2]
 ##[1,] 1.0158730 0.1269841
 ##[2,] 0.1269841 1.0158730
 

