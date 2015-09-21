#makeCacheMatrix should be used to create a matrix that is capable of caching its inverse
#cacheSolve should be used to operate on matrices created using makeCacheMatrix
# to efficiently get the inverse, returning the cached version where available


#makeCacheMatrix
# given a matrix x, return a list that contains 4 elements:
#   $set -> function to be used to set to a new matrix
#   $get -> function, get the stored matrix
#   $setinverse -> set the inverse matrix to a calculated value 
#   $getinverse -> retrieve the previously-calculated value

makeCacheMatrix <- function(x = matrix()) {
        cachedInverse <- NULL
        set <- function(newMatrix) {
                x <<- newMatrix
                cachedInverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) cachedInverse <<- inverse
        getinverse <- function() cachedInverse

        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#given a cache matrix, as created by makeCacheMatrix, return the previously-computed
# inverse of the matrix, or compute it, save and return it.
cacheSolve <- function(x, ...) {

	  #get the cached inverse of the matrix, check if it's been calculated
	  inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }

	  #have to calculate it, so get the matrix and store the inverse for next time
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse

}
