
# Together, makeCacheMatrix() and cacheSolve() let the caller compute
# the inverse of a square matrix and use the inverse multiple times,
# without the need to recompute it.
#
# This will save on expensive computation resurce, provided that the
# original matrix is not changed between the calls to cacheSolve().


##########
# makeCacheMatrix(x) will produce a cache object that will hold the
# matrix x and its inverse. In fact, its use does not need to be
# restricted to inverse of the input matrix.

makeCacheMatrix <- function(x = matrix()) {
	xinv <- NULL	# start with no cached inverse

	setdata <- function(d) {
		x <<- d
		xinv <<- NULL
	}

	getdata <- function() {
		x
	}

	setinv <- function(inv) {
		xinv <<- inv
	}

	getinv <- function() {
		xinv
	}

	list(	setdata = setdata,
		getdata = getdata,
		setinv  = setinv,
		getinv  = getinv )
}


##########
# cacheSolve() will return the inverse of the special matrix, x, as
# returned by makeCacheMatrix().

cacheSolve <- function(x, ...) {
	# check for and return the cached inverse
	xinv <- x$getinv()
	if (!is.null(xinv)) {
		message("returning cached inverse")
		return(xinv)
	}

	# no cached inverse! compute the inverse and cache it
	xdata <- x$getdata()
	xinv  <- solve(xdata)
	x$setinv(xinv)
	xinv
}
