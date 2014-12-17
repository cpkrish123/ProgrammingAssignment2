## Put comments here that give an overall description of what your
## functions do

## Author: C.P.Krishnan
## Course: Coursera R Programming

## Write a short comment describing this function
## Function Name: makeCacheMatrix
## Input Parameters: Takes in x - a Matrix (assumed to be invertable)
## Output: Returns the Inverse of the matrix

## makeCacheMatrix creates a special "vector", 
## which is really a list containing a function to
## set the value of the matrix
## get the value of the inverse matrix
## set the value of the inverse matrix
## get the value of the inverse Matrix
makeCacheMatrix <- function(x = matrix()) {
	## Initialize the Inverse matrix m that is returned to NULL
	m <- NULL
	
	## get the matrix that has been solved (inverse)
	get <- function() x

	## set function to the set the Matrix in a cached environment
	## also caches the Inverse Matrix to NULL in that cache.
	set <- function (y) { 
		## assigns value to object so it can be cached
		## in a different environment (not the same as current)
		x <<- y 
		m <<- NULL
	}

	## Function setmatrix solves the inverse of the matrix
	## sets the m (return value) to the solved matrix
	## this is done in the cached environment
	setmatrix <- function(solve) m <<- solve

	## Function getmatrix returns the inverse matrix calculated
	## in the cached environment
	getmatrix <- function() m

	### create the special "list"
	list (set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## Write a short comment describing this function
## calculates the inverse matrix of the special "vector" 
## created with the above function (makeCacheMatrix)
cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'

	## First check if the inverse is already calculated
	m <- x$getmatrix()
	## return that matrix if not null
	if (!is.null(m)) {
		message ("getting cached matrix")
		return (m)
	}

	## get the input matrix from cache
	inpMatrix <- x$get()

	## solve (get inverse of the input matrix)
	m <- solve(inpMatrix, ...)

	## set the matrix into the cache
	x$setmatrix(m)

	## return the inverse matrix
	m
}
