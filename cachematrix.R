## Write a short comment describing this function
## makeCacheMatrix:
## Return the special vector of four functions:
## 1. set: set the value of the vector
## 2. get: get the value of the vector
## 3. set_: set the value of the inverse
## 4. get_: get the value of the inverse

  
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) m <<- solve
	getsolve <- function() m
	list(set = set, get = get,setsolve = setsolve,getsolve = getsolve)
  }

##The following function calculates the mean of the special "vector" created with the above function.
##it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.
  
cacheSolve <- function(x, ...) {
	m <- x$getsolve()
	if(!is.null(m)){
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data,...)
	x$setsolve(m)
	m
}
