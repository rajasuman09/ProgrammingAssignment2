## Program to calculate the inverse of a given invertible matrix. If an attempt to calculate the inverse of the same invertible matrix,
## cache is checked first and returns the inverse if it is found, Else it will be re-calculated.

## Following function 'makeCacheMatrix()' takes an invertible matrix as input and builds a list of 4 functions which sets and gets the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {

inv <- matrix()
get <- function() x

set <- function(y) {
	x <<- y
	inv <<- NULL
}

setinverse <- function(inverse) inv <<- inverse

getinverse <- function() inv

list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## Below function takes the output list of above function as input and returns inverse either from cache or by calculating it if not found in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		inv <- x$getinverse()
		
		if(dim(inv)[1] == 1 && dim(inv)[2] == 1 && is.na(inv[1][1])) {
			
			message("Calculating inverse of a matrix")
			
			data <- x$get()
			inv <- solve(data)
			x$setinverse(inv)
			inv
		
		}
		else {
			message("Fetching inverse from Cache")
			return(inv)
		}
}
