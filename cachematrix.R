## Put comments here that give an overall description of what your
## functions do

## Cette fonction calcule l'inverse de la matrice spéciale créée par makeCacheMatrix.
## Si l'inverse a déjà été calculé (et que la matrice n'a pas changé), 
## elle récupère directement le résultat depuis la mémoire cache sans refaire le calcul.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


##Cette fonction prend l'objet 
##créé par makeCacheMatrix et effectue 
##l'opération de manière optimisée

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
