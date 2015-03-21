## Overall this program takes a square vector and returns the inverse
## of it through using the solve function or recalling the value from 
## the cache, which is created the first time it runs solve

## makeCacheMatrix takes input x and creates object s with 
## NULL value everytime it is called. There is also a get 
## function that returns the value of the original vector; 
## a setsolve function that stores the value of the superassignment; 
## getsolve that returns the cahed value in cacheSolve in 
## future accesses; and, lastly, the list fuction that is accessed 
## each time makeCacheMatrix is called and a new object is created. The
## list is of the internal functions. 

makeCacheMatrix <- function(x = matrix()) {
   s <- NULL
   set <- function(y) {
   	x <<- y
   	s <<- NULL
   }
   get <- function() { x } 
   setsolve <- function(solve)  { s <<- solve }
   getsolve <- function() { s } 
   list(get = get, setsolve = setsolve, getsolve = setsolve)
}


## Using the object x, which was created by makeCacheMatrix, cacheSolve
## accesses x adn gets the value of the solve. If the inverse was
## already cached, it will send the message "getting cached data" and
## return the inverse. If s is NULL, then it will run solve to find 
## the inverse and then cache the result.   

cacheSolve <- function(x, ...) {
	s <- x$getsolve()
	if(!is.null(s)) {
		message("getting cached data")
		return(s)
	}
	data <- x$get()
	s <- solve(data,...)
	x$setsolve(s)
	s
}
