## Below is a function that is used to create a special object that stores a matrix and cache's its inverse matrix.
## It creates a special "matrix", which is really a list containing a function to

    ## set and get the values of the matrix
    ## set and get the values of the inverse of the matrix

makeCacheMatrix <- function( mat = matrix()){

mat_inverse <- NULL;

# A function to set the matrix with user input
# from a different environment.
set <- function(y){
mat <<- y
mat_inverse <<- NULL
}

get <- function() mat

# Similarly, another function to set the matrix inverse
# from the user (from a different environment).
set_inverse <- function(inverse) mat_inverse <<- inverse

get_inverse <- function() mat_inverse

list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}


# This function calculates the inverse of the special "matrix" created with the "makeCacheMatrix" function.
# It first checks to see if the inverse has already been calculated.
# If so, it gets the inverse from the cache and skips the computation.
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the set_inverse function.


cacheSolve <- function(mat, ...) {
        
# Check whether the inverse of the matrix exists in the Cache Memory
mat_inverse <- mat$get_inverse()
        if(!is.null(mat_inverse)) {
# If the inverse exists in the Cache Memory, return it
                message("Getting Matrix from Cache Memory.")
                return(mat_inverse)
        }

# If the inverse doesn't exist, compute the inverse
# and store it in mat_inverse
        data <- mat$get()
        mat_inverse <- solve(data, ...)

# Update the Cache with the computed inverse of the matrix 'mat'
        mat$set_inverse(mat_inverse)

# Return the inverse of the matrix 'mat' to the user
        mat_inverse
}


