## This R script contains a pair of functions that cache the inverse of a matrix
## 	1) makeCacheMatrix and 
##	2) cacheSolve


## makeCacheMatrix:  This function creates a special "matrix" object that cache its inverse

makeCacheMatrix <- function(mtx = matrix()) 
{
	i <- NULL

		## Set the matrix
	set <- function(amtx)
		{
		mtx <<- amtx
		i <<- NULL
		}

		## Get the matrix
	get <- function() mtx

		## Set the inverse of the matrix
	setInverse <- function(solve) i <<- solve

		## Get the inverse of the matrix
	getInverse <- function() i

		## Return list
	list (set = set, get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}


## cacheSolve:  This function computes the inverse of the special "matrix" returned 
## 	by the makeCacheMatric function.  If the inverse has been calculated, then
##	this function should retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) 
{
		## Return a matrix that is the inverse of 'x'
	i <- x$getInverse()

		## Return the inverse if it has been calculated
	if(!is.null(i))
		{
		message("Pulling data from cache...")
		return(i)
		}

		## Get matrix object
	amtx <- x$get()
		## Calculate inverse of the matrix object
	i <- solve(amtx, ...)
		## Set inverse of the matrix object
	x$setInverse(i)

		## Return matrix
	i
}
