## ProgrammingAssignment 2 in R Programming
## How to cache an evaluated value and re-use it when needed


## Takes a matrix and caches its inverse 
makeCacheMatrix <- function(x = matrix()) {

	# Deklarerar resultatmatrisen som NULL
	res <- NULL
	
	# Funktionen 'setM' sparar inkommande matris som x och
	# nollställer reultatmatrisen.
	setM <- function(y) { 
	      x  <<- y 
	      res <<- NULL 
	}

	# Funktionen 'getM' returnerar ett matrisobjekt
	getM <- function() x

	# Funktionen 'setInverseMatrix' inverterar resultatmatrisen och sparar
	setInverseM <- function(solve) res <<- solve
	
	# Funktionen 'getInverseMatrix' returnerar den inverterade resultatmatrisen
	getInverseM <- function() res

	# Lista på interna funktioner
	list(setM=setM, 
	     getM=getM,
	     setInverseM = setInverseM,
	     getInverseM = getInverseM)
}



## Takes a matrix and returns its inverse
cacheSolve <- function(x, ...) {

	   # Hämta den inverterade versionen av inkommande x-matris
	   res <- x$getInverseM()
	   # Om den finns, dvs om res inte är tom,
	   if (! is.null(res)) {
	      # har vi hittat en cachad matrix
	      message("Found cached matrix")
	      # returnera den
	      return(res)
	   }
	
	   # Om matrisen INTE fanns sparad,
	   # skapa nytt matrisobjekt av x
	   newM <- x$getM()
	   # invertera objektet,
	   res <- solve(newM,...)
	   # spara det
	   x$setInverseM(res)
	   # och returnera det
	   res
}
