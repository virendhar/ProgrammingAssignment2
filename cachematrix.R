# Code: Written by Virendhar Sivaraman on 24 th Aug 2014 for Cloudera R Programming Week 2 Programming Assignemnt 
# Description :

# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.


# Code : makeCacheMatrix() Function does the following, 
# Description :  The function is used by the cacheSolve() to Set & Get value of the matrix and to set & Get 
#                inverse of the matrix


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

# Code : cacheSove() Function. 
# Description : The function returns the inverse of the matrix. It first checks if
#               the inverse has already been computed. If so, it gets the result and skips the
#               computation. If not, it computes the inverse, sets the value in the cache via setinverse function.


cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}


## Sample run:
## > x = rbind(c(1, -0.25), c(-0.25, 1))
## > SqMatrix = makeCacheMatrix(x)
## > SqMatrix$get()
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## No cache in the first run
## > cacheSolve(SqMatrix)
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667

## Retrieving from the cache in the second run
## > cacheSolve(SqMatrix)
## getting cached data.
##           [,1]      [,2]
## [1,] 1.0666667 0.2666667
## [2,] 0.2666667 1.0666667
## > 



