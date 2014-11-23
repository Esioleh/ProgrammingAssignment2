## This function creates an object of the "list" type. This object stores
## the matrix value and the cached value of its inverse. There are 4 
## function in order to change the values and to read them.
## makeCacheMatrix creates the new object that will be used by the second
## function cacheSolve. makeCacheMatrix creates object and then allow
## to access that object not the matrix.

## cacheSolve() accesses the object created with the first function.
## If it is the first time we call a matrix the function compute inverse
## If it is 2nd or more access the function fetch the stored value

makeCacheMatrix <- function(x = matrix()) {
        s<-NULL    ## s will be the 'inverse' and it's reset to NULL when function is called
        set<-function(newValue){
                x<<- newValue
                s<<- NULL
        }
        get<-function()x   ## returns the original matrix value
        setinverse<-function(inverse) {   ## this function is called by cacheSolve()
                s <<- inverse}
        getinverse<-function() {s}   ## return the cached value to cacheSolve() after first access
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

        

cacheSolve <- function(x, ...) {  ## the input is the object created by makeCacheMatric
       s <- x$getinverse()
       if(!is.null(s)) {
               message("getting cached data")
               return(s)         
       }
       data <- x$get()
       s<- solve(data, ...)
       x$setinverse(s)
       s                   
}
