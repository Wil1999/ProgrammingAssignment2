## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    x_inv <- NULL
    set <- function(y){
        x_inv <<- y
        x_inv <<- NULL
    }
    get <- function() x
    setInv <- function(inver) x_inv <<- inver
    getInv <- function() x_inv
    list(set = set,get = get,
         setInv = setInv,
         getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)){
        message("Obteniendo datos del caché")
        return(inv)
    }
    dato <- x$get()
    if(det(dato) == 0){
        message("La matriz no es cuadrada o no es invertible")
        return(dato)
    }else{
        inv <- solve(dato)
    }
    x$setInv(inv)
    inv
}
