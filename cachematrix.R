## The function catches matric inverse value
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(matrx){
        if (!identical(matrx, x)){
            x <<- matrx
            inv <<- NULL
        }
    }
  
    get <- function(){
      x
    }
  
    setinverse <- function(inverted){
        inv <<- inverted
    }
  
    getinverse <- function(){
      inv
    }
  
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The function that resolves the caches value
cacheSolve <- function(x, ...) {
        
        inv <- x$getinverse()
        
        if (!is.null(inv)){
            return (inv)
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        
        x$setinverse(inv)
        
        inv
}