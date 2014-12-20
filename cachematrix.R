makeCacheMatrix <- function(x = matrix()) {  # x will be a matrix

        m <- NULL           # m will be the inverse and it will be reset
                            # to NULL everytime makeCacheMatrix() is called
                            
        set <- function(y) { # takes an input matrix

                x <<- y      # stores its value in x

                m <<- NULL   # and resets the inverse m to NULL
        }
        get <- function() x  # it returns the original matrix

        setsolve <- function(solve){ # it is called by cacheSolve() function

                m <<- solve  # it sets the value of m to the inverse of the 
                             # matrix passed in cacheSolve()
        }                                         
        getsolve <- function() m # it returns cached value to cacheSolve

        list(set = set, get = get, # makeCacheMatrix() returns an object of 
             setsolve = setsolve,  # type list
             getsolve = getsolve)
}

cacheSolve<- function(x, ...) { # the input x has been taken from 
					  # makeCacheMatrix()
        m <- x$getsolve()   # stores the value of inverse in m accessed 
				    # from object x

        if(!is.null(m)) {   # if the inverse has already been calculated
                            # for a particular matrix

                message("getting cached data") # prints the message on the console
                
                return(m)    # returns the inverse              
        }
        data <- x$get()      # if m is NULL then this code is accessed

        m <- solve(data, ...) # calculates the inverse

        x$setsolve(m)  # stores the calculated value in object x

        m        # returns the inverse
}

