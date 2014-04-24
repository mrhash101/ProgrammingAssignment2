##how to run
## initiate a square matrix and store it in a variable. for. e.g, myMat<- matrix(1:4, ncol = 2, nrow = 2)
## make functions by calling makeCacheMatrix(myMat) and store the result in another variable, for. e.g, res<- makeCacheMatrix(myMat)
## call cacheSolve with res, for. e.g, cacheSolve(res) and voila... you have your answer



## makeCacheMatrix contains the getters and setters with global variable for the inverse of matrix. 
## cacheSolve has the capability to check if an inverse of a matrix has already been calculated. 
## done on 23rd of April, 2014

## Getters and setters for the inverse of x

makeCacheMatrix <- function(x = matrix()) {
  
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  
}


## cacheSolve works on the same principle as the example provided - it checks if the inverse of a square matrix
## is already there - if yes, it gets the values, otherwise calculates and sets them

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' stored in 'm'
  
    #x<-as.data.frame(x)
    m <- x$getinv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...) 
    x$setinv(m)
    m
  
}
