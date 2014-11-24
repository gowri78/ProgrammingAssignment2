##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

# function takes Matrix as an input
# creates an object with  four methods/functions
# 2 set Methods/functions of this Object  sets  matrix and its inverse into Cache
# another 2 get methods/functions of this object retrives matrix and its inverse from Cache
# returns all four methods as list
makeCacheMatrix <- function(x = matrix()) {
        ## Assigning local variable to NULL
        m <- NULL
        
        ## Defining a set function which stores the passed argument matrix in the Cache
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ##Defining get Function which retrives the matrix 
        get <- function() x
        
        ##Defining a solve function which stores the inverse of matrix in cache
        setsolve <- function(solve) m <<- solve
        
        ##Defining get Function which retrives the inverse of matrix stored in Cache 
        getsolve <- function() m
        
        ## returning a list consists of methods/functions which sets & retrives matrix and its inverse to/from cache
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
        
        
}

##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

# function takes matrix as an input 
#  Retrives & checks the inverse matrix cached in the memory
# if not null then returns the inverse
# if cached inverse is null calculates inverse using solve
#
# returns the inverse of matrix 
cacheSolve <- function(x, ...) {
        
        ## Retriving  the cached inverse of the matrix
        m <- x$getsolve()
        
        ##Checking in cached inverse is not null 
        if(!is.null(m)) {
                message("getting cached data")
                ## Cached inverse matrix is not null hence returning the inverse
                return(m)
                ## function terminates
        }
        ## retriving the cached matrix and assigning it to variable/matrix
        data <- x$get()
        
        ##calcualting the  inverse of matrix 
        m <- solve(data, ...)
        
        ## storing the calculated inverse matrix to cache
        x$setsolve(m)
        
        ##returns the calculated inverse matrix
        m
}
