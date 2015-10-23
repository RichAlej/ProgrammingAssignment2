## These two functions take a matrix and associate with functions that allow
##you to calculate and store the value of the matrix's inverse; this allows
##you to save computation time for this complex task in the case that you need
##to repeatedly refer to the Inverse of a matrix


## makeCacheMatrix() takes a matrix as its argument and associates it with 
##a list of functions that can be used to store and recall the value of the matrix 
##and the value of its inverse -- a "cached matrix"

makeCacheMatrix <- function(x = matrix()) {

        I <- NULL    #I is for inverse
        set <- function(y) {
                x <<- y        # sets x for out of local environment, or globally
                I <<- NULL    
        }
        get <- function() x   
        setInverse <- function(invrs) I <<- invrs  #sets I value globally, for storage        
        getInverse <- function() I  
        list(set = set, get = get, 
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve() takes a "cached matrix" (matrix called through makeCacheMatrix)
##as its argument and returns the inverse; 
##it calculates the inverse if it has not been calculated;
##if the inverse has already been calculated by cacheSolve(), then it returns the value 
##that has been set (or cached) in the matrix  

cacheSolve <- function(x, ...) {
        I <- x$getInverse()    
        if(!is.null(I)) {                        #checks for stored value
                message("getting cached data")
                return(I)                       
        }
        data <- x$get()           #retrieves the value of x matrix
        I <- solve(data, ...)     #calculates inverse
        x$setInverse(I)         #stores the inverse value and associates with x
        I
}
