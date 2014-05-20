## This matrix will be used as data for inverting a matrix.
m <- matrix(sample(1:100, 100), 10, 10)

## This function takes a matrix and creates a "special" matrix to which a
## list of functions (set, get, set_inv, get_inv) can be applied through the
## subsequent cacheSolve function.
makeCacheMatrix <- function(x = matrix()) {
        inv_matrix <- NULL
        set <- function(y){
                x <<- y
                inv_matrix <<- NULL
        }
        get <- function(){
                x
        }
        set_inv <- function(inv){
                inv_matrix <<- inv
        }
        get_inv <- function(){
                inv_matrix
        }
        list(set=set, get=get, set_inv=set_inv, get_inv=get_inv)
}

## This creates the "special" matrix using matrix m as data.
special <- makeCacheMatrix(m)

## This function takes "special" as an argument and if there is no cached
## inverted matrix (special$get_inv is NULL) it calculates the inverted matrix,
## otherwise it checks what the cached inverted matrix is and returns it. (I
## deliberately reordered the if/else statements in the example to avoid the
## !is.null which I find awkward.)
cacheSolve <- function(x, ...) {
        solved_matrix <- x$get_inv()
        if(is.null(solved_matrix)){
                mat <- x$get()
                inv <- solve(mat)
                x$set_inv(inv)
                inv
        } else {
                message ("getting cached inverted matrix")
                return(solved_matrix)
        }
}

## This passes "special" through cacheSolve and returns the inverted matrix of m
cacheSolve(special)
