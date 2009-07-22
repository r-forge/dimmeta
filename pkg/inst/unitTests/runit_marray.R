################################################################################
# R PACKAGE:   metadata
# FILE:        R/runit_marray.R
# DESCRIPTION: Test suite for the 'marray' S3 class. To be run with svUnit. 
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  2009 July 7
# LICENSE:     GPL-2
################################################################################

#y <- as.array(x)
#dimnames(y) <- list(paste("r", 1:2, sep=""), paste("c", 1:3, sep="")) 

test_marray_withoutDimattr <- svTest(function() {		
	x <- marray(1:6, dim=c(2, 3))
	
	checkTrue(inherits(x, "marray"));
	checkTrue(is.array(x));
	checkTrue(is.marray(x));
	
	checkEquals(c(2L, 3L), dim(x));
	checkEquals(NULL, dimnames(x));
	checkEquals(NULL, dimdata(x));
})

test_simple_marray <- svTest(function() {	
	xda <- list(letters[1:2], LETTERS[1:3]);
	x <- marray(1:6, dim=c(2, 3), dimdata=xda);
	
	checkTrue(inherits(x, "marray"));
	checkTrue(is.array(x));
	checkTrue(is.marray(x));
	
	checkEquals(xda, dimdata(x));
})

# If 'dim' is NOT preseved, 'dimdata' isn't, neither.
# Note that when using more than one index and drop=TRUE and a vector
#	is returned, 'dimnames' is transformed to 'names'. What should we
#	do here? A 1-d array? A new class for 'vector'? A standard 'vector'
#   with an extra attribute?

test_extract_2d_dropDim <- svTest(function() {
	xda <- list(letters[1:2], LETTERS[1:3]);
	x <- marray(1:6, dim=c(2, 3), dimdata=xda);
	
	# Using a single argument indexes as a vector, irrespective of 'drop' value 
	checkEquals(2, x[2,drop=FALSE]);
	checkEquals(2, x[2,drop=TRUE]);

	# With drop=TRUE and returning a vector, dimdata is dropped		
	checkEquals(c(5, 6), x[,3]);	
	checkEquals(c(1, 3, 5), x[1,]);
	checkEquals(c(6, 2), x[2, c(3, 1)]);
})

test_extract_2d_keepDim <- svTest(function() {
	xda <- list(letters[1:2], LETTERS[1:3]);
	x <- marray(1:6, dim=c(2, 3), dimdata=xda);
	
	# No indexes return the object unchanged
	checkEquals(x, x[]);
    	
	# With drop=FALSE and 2 indexes, keep dimdata		
	checkEquals(marray(6, dim=c(1, 1), dimdata=list("b", "C")), 
		x[c(FALSE, TRUE), c(FALSE, FALSE, TRUE), drop=FALSE]);
	
	# With drop=FALSE and one index missing, keep dimdata		
	checkEquals(marray(c(2, 1, 4, 3, 6, 5), dim=c(2, 3), 
			dimdata=list(c("b", "a"), c("A", "B", "C"))), 
		x[2:1,]);	
		
	# Select an array with one column less, resorting rows and columns	
	checkEquals(marray(c(6, 5, 2, 1), dim=c(2, 2), 
			dimdata=list(c("b", "a"), c("C", "A"))), 
		x[2:1, c(3, 1)]);
	
	# Extract using names
	dimnames(x) <- list(paste("r", 1:2, sep=""), paste("c", 1:3, sep="")); 
	checkEquals(marray(c(4, 3, 2, 1), dim=c(2, 2),
    		dimnames=list(c("r2", "r1"), c("c2", "c1")),
			dimdata=list(c("b", "a"), c("B", "A"))), 
		x[c("r2", "r1"), c("c2", "c1")]);
	
	# Extract using matrix: always drops dimensions, coercing to vector 
	checkEquals(c(6, 6, 3), x[matrix(c(2, 2, 1, 3, 3, 2), 3, 2)]);
})

test_extract_2d_indexEdgeCases <- svTest(function() {
	xda <- list(letters[1:2], LETTERS[1:3]);
	x <- marray(1:6, dim=c(2, 3), dimdata=xda);
	
	checkEquals(marray(integer(0), dim=c(0,0), 
			dimdata=list(character(0), character(0))),
		x[NULL, NULL]);

	checkEquals(marray(integer(0), dim=c(0,0), 
			dimdata=list(character(0), character(0))),
		x[integer(0), logical(0)]);

	checkEquals(marray(integer(0), dim=c(0,0), 
			dimdata=list(character(0), character(0))),
		x[0, 0]);

	checkEquals(marray(integer(0), dim=c(2,0), 
			dimdata=list(c("a", "b"), character(0))),
		x[, 0]);
	
	checkEquals(marray(NA_integer_, dim=c(2, 3), 
			dimdata=list(rep(NA_character_, 2), c("A", "B", "C"))),
		x[NA, ]);	
})

test_extract_2d_dimdataNonVector <- svTest(function() {
	xda <- list(data.frame(X1=-1:-2, X2=factor(c("eur", "usd"))), 
		list(X=1, Y=5:3, Z=list("wow!", "ja!")));
	x <- marray(1:6, dim=c(2, 3), dimdata=xda);
	
	checkEquals(marray(c(6, 5), dim=c(2, 1), 
			dimdata=list(
				data.frame(X2=factor(c("eur", "usd")), X1=-1:-2), 
				list(Z=list("wow!", "ja!"))
			)),		
		x[2:1, 3, drop=FALSE]) 	
})

test_extract_dimdata <- svTest(function() {
	xda <- list(data.frame(-1:-2, factor(c("eur", "usd"))), 
		list(X=1, Y=5:3, Z=list("wow!", "ja!")));
	x <- marray(1:6, dim=c(2, 3), dimdata=xda,
		dimnames=list(D1=paste("r", 1:2, sep=""), D2=paste("c", 1:3, sep="")));
	
	checkEquals(xda, dimdata(x));
 	checkEquals(
		list(D1=data.frame(r1=-1:-2, r2=factor(c("eur", "usd"))), 
			D2=list(c1=1, c2=5:3, c3=list("wow!", "ja!"))),		
		dimdata(x, use.dimnames=TRUE));
})

test_rbind <- svTest(function() {
	# With vectors --elements without metadata become NA
	x <- marray(1:6, dim=c(2, 3), dimdata=list(letters[1:2], LETTERS[1:3]));
	y <- marray(10, dim=c(1, 3), dimdata=list("j", c("X1", "X2", "X3")));
	
	checkEquals(marray(c(1, 2, 10, 3, 4, 10, 5, 6, 10), dim=c(3, 3), 
			dimdata=list(c("a", "b", "j"), LETTERS[1:3])),
		rbind(x, y));

	checkEquals(marray(c(1, 1, 2, 1, 3, 4, 1, 5, 6), dim=c(3, 3), 
			dimdata=list(c(NA_character_, letters[1:2]), LETTERS[1:3])),
		rbind(1, x));
		
	# With lists --elements without metadata become list(NULL)
	xl <- marray(1:6, dim=c(2, 3), 
		dimdata=list(list("a", "b"), list("A", "B", "C")));

	checkEquals(marray(c(1, 2, 1, 2, 3, 4, 3, 4, 5, 6, 5, 6), dim=c(4, 3), 
			dimdata=list(list("a", "b", "a", "b"), list("A", "B", "C"))),
		rbind(xl, xl));

	checkEquals(marray(c(1, 1, 2, 1, 3, 4, 1, 5, 6), dim=c(3, 3), 
			dimdata=list(list(NULL, "a", "b"), list("A", "B", "C"))),
		rbind(1, xl));
	
	# Join lists and vectors --they are coerced to list
	checkEquals(marray(c(1, 2, 2, 1, 2, 3, 4, 2, 3, 4, 5, 6, 2, 5, 6), 
			dim=c(5, 3), 
			dimdata=list(list("a", "b", NULL, "a", "b"), list("A", "B", "C"))),
		rbind(xl, 2, x));	
		
})

test_cbind <- svTest(function() {
	# With vectors --elements without metadata become NA
	x <- marray(1:6, dim=c(2, 3), dimdata=list(letters[1:2], LETTERS[1:3]));
	y <- marray(10, dim=c(2, 1), dimdata=list(c("j", "w"), "X1"));
	
	checkEquals(marray(c(1, 2, 3, 4, 5, 6, 10, 10), dim=c(2, 4), 
			dimdata=list(c("a", "b"), c("A", "B", "C", "X1"))),
		cbind(x, y));

	checkEquals(marray(c(1, 1, 1:6), dim=c(2, 4), 
			dimdata=list(letters[1:2], c(NA_character_, "A", "B", "C"))),
		cbind(1, x));
		
	# With lists --elements without metadata become list(NULL)
	xl <- marray(1:4, dim=c(2, 2), 
		dimdata=list(list("w", "j"), list("X1", "X2")));

	checkEquals(marray(c(1:4, 1:4), dim=c(2, 4), 
			dimdata=list(list("w", "j"), list("X1", "X2", "X1", "X2"))),
		cbind(xl, xl));

	checkEquals(marray(c(1:4, -10, -20), dim=c(2, 3), 
			dimdata=list(list("w", "j"), list("X1", "X2", NULL))),
		cbind(xl, c(-10, -20)));
	
	# Join lists and vectors --they are coerced to list
	checkEquals(marray(c(1:4, 2, 2, 1:6), 
			dim=c(2, 6), 
			dimdata=list(list("w", "j"), list("X1", "X2", NULL, "A", "B", "C"))),
		cbind(xl, 2, x));	
		
})