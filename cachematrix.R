

## Esta es una función que crea una matriz especial que puede guardar en caché su inversa

makeCacheMatrix <- function(x = matrix()) {
  invr <- NULL
  set <- function(y) {
    x <<- y
    invr <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invr <<- inverse
  getinverse <- function() invr
  list(set=set, get=get, setinverse=setinverse, getinverse = getinverse)
}


## Esta función devuelve la inversa de una matriz cuadrada invertible

cacheSolve <- function(x, ...) {
  invr <= x $ getinverse ()
  if (! is.null (invr)) {
    message ("Getting Cached Data: -")
    return (invr)
  }
  matrx <- x $ get ()
  invr <- solve (matrx, ...)
  x $ setinverse (invr)
  invr
}
