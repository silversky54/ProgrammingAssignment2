##Code by: Erick Gonzalez
##last Change: 10/26/2014
##NOTE: Programming Assignment 2 

## cachematrix.R has two functions makeCacheMatrix and cachesolve,
## these will calculate and store the inverse of a Matrix to avoid
## having to re-compute the same value over and over for the same 
## matrix


## makeCacheMatrix creates a special vector with functions to 
## set the matrix, get the stored matrix, set or store the inverse of 
## the matrix and get or see the stored inverse of the matrix. 

makeCacheMatrix <- function(x = matrix()) {
        iom <- NULL ##iom: Inverse of Matrix
        set <- function(y) {
                x <<- y
                iom <<- NULL 
        }
        get <- function() x
        setinverse <- function(inverse) iom <<- inverse
        getinverse <- function() iom
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cachesolve is a function that takes a matrix "x", calculates
## it's inverse and stores the result for future reference 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        iom <- x$getinverse()
        if(!is.null(iom)) {
                message("getting cached data")
                return(iom)
        }
        
        data <- x$get()
        ##Verify x is a square Matrix and calculate it's inverse
        if(nrow(data)==ncol(data)){
                message("Calculating data ")
                iom<- solve(data, ...)
                x$setinverse(iom)
                iom        
        }
        else{
                message("Data is not a square Matrix ")
                ##return()
        }
        
}

