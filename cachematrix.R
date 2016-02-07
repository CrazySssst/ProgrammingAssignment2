## Put comments here that give an overall description of what your
## functions do

## create a object which can store a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	## set  original matrix
	set <- function(y){
		   x <<- y
		   inv <<- NULL
	}
	## get original matrix
	get <- function() x
	## set inverse of original martix
	setInv <- function(inverse) inv <<- inverse
	## get inverse of original martix
	getInv <- function() inv
	list(set=set,get=get,setInv=setInv,getInv=getInv)
}


## if inv is null, the function will calculate inverse of original martix
## otherwise,the function return inv directlly

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <- x$getInv()
	## if inv has been calculated
	if(!is.null(inv)){
		message("getting cached inverse matrix")
	}
	## calculate inverse of original matrix
	data <- x$get()
	inv <- solve(data,...)
	## store inv
	x$setInv(inv)
	inv
}
