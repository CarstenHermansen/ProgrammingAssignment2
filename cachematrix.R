## CacheMatrix calculates the inverse of an invertible square matrix.
## It then caches this solution to save time on a computation of the same matrix later.

## makeCacheMatrix makes a list with the matrix information. Firstly, define a variable
## as the empty function (fx. smat <- makeCacheMatrix()). Then pass the matrix to the
## "set" element of the list (fx. smat$set(x) # where 'x' is the matrix). You can use
## the smat$get to get the matrix again, smat$setsol to set the solve (inverse) manually
## and smat$getsol to get the inverse after it has been set.

makeCacheMatrix <- function(x = matrix()) {
        sol <- NULL
        set <- function(y) {
                x <<- y
                sol <<- NULL
        }
        get <- function() x
        setsol <- function(solve) sol <<- solve
        getsol <- function() sol
        list(set = set, get = get, setsol = setsol, getsol = getsol)
}


## cacheSolve takes the matrix-info-list as an argument (smat, from earlier
## example: cacheSolve(smat)). The function then checks the smat$getsol, to
## see if the solution is already stored. If it is, it gives a massage, and
## returns the inverse from the cache. If not, the function computes the
## inverse, and uses the $setsol element to store the solution. Running the
## line again will result in a succesful attempt at getting the cached value,
## generating the message and saving time on the computation.

cacheSolve <- function(x, ...) {
        sol <- x$getsol()
        if(!is.null(sol)){
                message("getting cached data")
                return(sol)
        }
        data <- x$get()
        sol <- solve(data, ...)
        x$setsol(sol)
        sol
}
