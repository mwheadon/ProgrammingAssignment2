## The following functions, when used together, will store the values of a 
## matrix and its inverse in a "special matrix." 
## This "special matrix" is simply a list containing functions for getting
## and storing the values. 


## makeCacheMatrix() generates a "special matrix", which is really a list of ## functions for storing data. The functions of makeCacheMatrix() store the ## value of its raw inputed matrix, as well as, any cached value provided by 
## another function (e.g. cacheSolve())

makeCacheMatrix <- function(x = matrix()) {
    mymatrix <- NULL        
    set <- function(y) {    
        x <<- y     
        mymatrix <<- NULL
    }
    get <- function() x     
    setmatrix <- function(solve) mymatrix <<- solve     
    getmatrix <- function() mymatrix    
    list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix) 
}


## cacheSolve() returns the inverse of the raw data stored in a "special 
## matrix." However, when used iteratively, cacheSolve() will only compute 
## and inverse for a given "special matrix" once. This is because it stores 
## its output in the "special matrix" given as input, and will check if there ## is a stored inverse before computing one. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    mymatrix <- x$getmatrix()
    if(!is.null(mymatrix)) {
        message("getting cached matrix")
        return(mymatrix)
    }
    data <- x$get()
    mymatrix <- solve(data, ...)
    x$setmatrix(mymatrix)
    mymatrix
}
