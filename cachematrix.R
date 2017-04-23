## Put comments here that give an overall description of what your
## functions do

## This function makes a special "matrix" that cache its inverse. It set the matrix, get the matrix, set the inverse and get the inverse 


makeCacheMatrix <- function(x = matrix()) {     
        inverse <- NULL                         
        set <- function(y) {  
                x <<- y  
                inverse <<- NULL  
        }  
        get <- function() x  
        setinverse <- function(inverse) inverse <<- inverse  
        getinverse <- function() inverse  
        list(set = set, 
             get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse has already been calculated the cacheSolve will retrieve the inverse from cache.  
        

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse() 
        if (!is.null(inverse)) {  
                message("getting cached data")  
                return(inverse)  
        }  
        data <- x$get()  
        inverse <- solve(data)  
        x$setinverse(inverse)  
        inverse
} 

