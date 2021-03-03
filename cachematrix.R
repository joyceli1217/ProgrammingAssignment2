## the first function create a vector of functions which 1)set a matrix 2)get the matrix 3)set the inverse 4)get the inverse
## The following function calculates the inverse of the special "vector" created with the above function 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        get <- function() x
	## function to calculate and fix inverse   
        setinverse <- function(inverse) inver <<- inverse    
	## function to return inverse
 	getinverse <- function() inver     
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
       
       inver <- x$getinverse()  
	##check if getinverse function will return Null or not
        if(!is.null(inver)) {
                message("getting cached data")
                return(inver)
        }
        data <- x$get()
        inverse <- solve(data, ...) 
	##if Null then calculate inverse  
        x$setinverse(inver)
        inver

}

