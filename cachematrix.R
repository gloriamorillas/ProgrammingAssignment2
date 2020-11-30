## we want to catch the inverse of a matrix, so that every time we want to do an inverse of 
## the same matrix, we do not have to compute the matrix inversion again. We can retrieved it
## and we can save valuable computing time. To do that, we need to run two functions.
## The first function will create an object where the inverse of a matrix will be cached.
## The second function will compute the inverse of a matrix and that it can be retrieved later. 




## The first function creates a variable "m" where the inverse of a matrix will be cached. 
## Then with set and get functions it links "m" with the parent environment, so that every time we put 
## another matrix (x), it is reseted and the inverse of the new matrix can be calculated.
## With the two levels of set and gets what we want to do is define "x" and "m" to the variable that 
## contains the function. Also, we create a list with the named sets and gets so it is easier to access 
## them when using the function later.


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinversematrix <- function(solve) m <<- solve
        getinversematrix <- function() m
        list(set = set, 
             get = get,
             setinversematrix = setinversematrix,
             getinversematrix = getinversematrix)
}




## With our second function, we want to retrieve the inverse of the matrix.
## The getinversematrix will get the cached inverted matrix. If the value of the 
## variable m is not NULL, then the function will retrieve the cached inverted 
## matrix from the parent environment. If the variable m is truly NULL, then the
## function will get a new matrix that we set previously and will calculate the 
## inverse of it. 


cacheSolve <- function(x, ...) {
        m <- x$getinversematrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinversematrix(m)
        m
}
