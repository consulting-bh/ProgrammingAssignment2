## The two functions in this code combine to:
## -create a matrix object
## -allow access to the object in order to set the value, retrieve the value.
## -calculate the inverse of the matrix and store the result in a "cache" variable
## -If the inverse has already been calculated, the previous result will be retrieved
##  without being recalculated, so as to save on processing time
## The functions are called by e.g.
## 1) selecting a matrix to invert :mat<-rbind(1:3,c(0,1,5),c(5,6,0))
## 2) calling CacheMatrix<-makeCacheMatrix(mat)
## 3) calling cacheSolve(CacheMatrix)


## makeCacheMatrix creates a special "matrix" and stores the matrix as well as its inverse
## The function is a list of four functions, which are evaluated when called by the cacheSolve function 
## The set and setinverse functions return the the matrix and inverse of the matrix respectively 


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y    
                m <<- NULL
        }
        get <- function() x  #assigns the function parameter x to an the variable calling "get"
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## This function reads the cached value of the inverse of 'x'. If there
## is no cached value, the function calculates the inverse using the 'solve() function,
## and stores the result in the cache variable m.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## data <- x$get() calls the function x$get (returns the result of the function, not the function itself)
        ## this line evaluates the function 'get' created by x<-makeCacheMatrix(mat)
        ## where mat is an invertible, square matrix. The result is that data is set equal to the
        ## matrix 'mat'
        data <- x$get()  
        m <- solve(data)
        
        x$setinverse(m) # runs the setinverse function which caches the result of m into the m variable
        m
}