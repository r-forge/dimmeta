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
	checkEquals(NULL, dimmeta(x));
})

test_simple_marray <- svTest(function() {	
	xda <- list(letters[1:2], LETTERS[1:3]);
	x <- marray(1:6, dim=c(2, 3), dimmeta=xda);
	
	checkTrue(inherits(x, "marray"));
	checkTrue(is.array(x));
	checkTrue(is.marray(x));
	
	checkEquals(xda, dimmeta(x));
})

# If 'dim' is NOT preseved, 'dimmeta' isn't, neither.
# Note that when using more than one index and drop=TRUE and a vector
#	is returned, 'dimnames' is transformed to 'names'. What should we
#	do here? A 1-d array? A new class for 'vector'? A standard 'vector'
#   with an extra attribute?

test_extract_2d_dropDim <- svTest(function() {
	xda <- list(letters[1:2], LETTERS[1:3]);
	x <- marray(1:6, dim=c(2, 3), dimmeta=xda);
	
	# Using a single argument indexes as a vector, irrespective of 'drop' value 
	checkEquals(2, x[2,drop=FALSE]);
	checkEquals(2, x[2,drop=TRUE]);

	# With drop=TRUE and returning a vector, dimmeta is dropped		
	checkEquals(c(5, 6), x[,3]);	
	checkEquals(c(1, 3, 5), x[1,]);
	checkEquals(c(6, 2), x[2, c(3, 1)]);
	
	# Reduce the number of dimensions
	x <- marray(1:8, dim=c(2, 2, 2), 
		dimmeta=list(letters[1:2], LETTERS[1:2], 1:2));
 
	checkEquals(marray(5:8, dim=c(2, 2), 
			dimmeta=list(letters[1:2], LETTERS[1:2])),
		x[,,2]);
})

test_extract_2d_keepDim <- svTest(function() {
	xda <- list(letters[1:2], LETTERS[1:3]);
	x <- marray(1:6, dim=c(2, 3), dimmeta=xda);
	
	checkEquals(x, x[], "no index returns the marray unchanged");
    	
	checkEquals(marray(6, dim=c(1, 1), dimmeta=list("b", "C")), 
		x[c(FALSE, TRUE), c(FALSE, FALSE, TRUE), drop=FALSE],
		"With drop=FALSE and 2 indexes, keep dimmeta");
	
	checkEquals(marray(c(2, 1, 4, 3, 6, 5), dim=c(2, 3), 
			dimmeta=list(c("b", "a"), c("A", "B", "C"))), 
		x[2:1,],
		"With drop=FALSE and one index missing, keep dimmeta");	
		
	checkEquals(marray(c(6, 5, 2, 1), dim=c(2, 2), 
			dimmeta=list(c("b", "a"), c("C", "A"))), 
		x[2:1, c(3, 1)],
		"select an array with one column less, resorting rows and columns");
	
	dimnames(x) <- list(paste("r", 1:2, sep=""), paste("c", 1:3, sep="")); 
	checkEquals(marray(c(4, 3, 2, 1), dim=c(2, 2),
    		dimnames=list(c("r2", "r1"), c("c2", "c1")),
			dimmeta=list(c("b", "a"), c("B", "A"))), 
		x[c("r2", "r1"), c("c2", "c1")],
		"extract using names (character vector subscript)");
	
	checkEquals(c(6, 6, 3), x[matrix(c(2, 2, 1, 3, 3, 2), 3, 2)],
		"extract using matrix always drops dimensions, coercing to vector");
})

test_extract_2d_indexEdgeCases <- svTest(function() {
	xda <- list(letters[1:2], LETTERS[1:3]);
	x <- marray(1:6, dim=c(2, 3), dimmeta=xda);
	
	checkEquals(marray(integer(0), dim=c(0,0), 
			dimmeta=list(character(0), character(0))),
		x[NULL, NULL]);

	checkEquals(marray(integer(0), dim=c(0,0), 
			dimmeta=list(character(0), character(0))),
		x[integer(0), logical(0)]);

	checkEquals(marray(integer(0), dim=c(0,0), 
			dimmeta=list(character(0), character(0))),
		x[0, 0]);

	checkEquals(marray(integer(0), dim=c(2,0), 
			dimmeta=list(c("a", "b"), character(0))),
		x[, 0]);
	
	checkEquals(marray(NA_integer_, dim=c(2, 3), 
			dimmeta=list(rep(NA_character_, 2), c("A", "B", "C"))),
		x[NA, ]);	
})

test_extract_2d_dfDimmeta <- svTest(function() {
	xda <- list(data.frame(X1=c(-1, -2), X2=factor(c("eur", "usd"))), 
		list(X=1, Y=5:3, Z=list("wow!", "ja!")));
	x <- marray(1:6, dim=c(2, 3), dimmeta=xda);
	
	checkEquals(marray(c(6, 5), dim=c(2, 1), 
			dimmeta=list(
				data.frame(X1=c(-1, -2), X2=factor(c("eur", "usd")))[
					c(2,1),,drop=FALSE], 
				list(Z=list("wow!", "ja!"))
			)),		
		x[2:1, 3, drop=FALSE],
		"use data frame and list with each item a different lengths as dimmeta");
 	
	checkException(marray(1:6, dim=c(2, 3), dimmeta=list(
			data.frame(X1=-1:-3, X2=factor(c("eur", "usd", "eur"))), 
			list(X=1, Y=5:3, Z=list("wow!", "ja!"))
		)), 
		"number of rows of data frame does not equal number of columns in array");
})

test_extract_dimmeta <- svTest(function() {
	xda <- list(data.frame(A1=-1:-2, A2=factor(c("eur", "usd"))), 
		list(X=1, Y=5:3, Z=list("wow!", "ja!")));
	x <- marray(1:6, dim=c(2, 3), dimmeta=xda,
		dimnames=list(D1=paste("r", 1:2, sep=""), D2=paste("c", 1:3, sep="")));
	
	checkEquals(xda, dimmeta(x));
 	checkEquals(
		list(D1=data.frame(A1=-1:-2, A2=factor(c("eur", "usd")), 
				row.names=c("r1", "r2")), 
			D2=list(c1=1, c2=5:3, c3=list("wow!", "ja!"))),		
		dimmeta(x, use.dimnames=TRUE));
	checkEquals(data.frame(A1=-1:-2, A2=factor(c("eur", "usd"))), rowmeta(x));
	checkEquals(data.frame(A1=-1:-2, A2=factor(c("eur", "usd")), 
			row.names=c("r1", "r2")), 
		rowmeta(x, use.rownames=TRUE));	
	checkEquals(list(X=1, Y=5:3, Z=list("wow!", "ja!")), colmeta(x));
	checkEquals(list(c1=1, c2=5:3, c3=list("wow!", "ja!")), 
		colmeta(x, use.colnames=TRUE));
	
	# When some dimension data is NULL...
	x <- marray(1:6, dim=c(2, 3), 
		dimnames=list(c("r1", "r2"), c("c1", "c2", "c3")), 
		dimmeta=list(letters[1:2], NULL));
	checkEquals(list(c("a", "b"), NULL), dimmeta(x));
	checkEquals(list(setNames(c("a", "b"), c("r1", "r2")), NULL), 
		dimmeta(x, use.dimnames=TRUE));
	checkEquals(c("a", "b"), rowmeta(x));
	checkEquals(setNames(c("a", "b"), c("r1", "r2")), 
			rowmeta(x, use.rownames=TRUE));
	checkEquals(NULL, colmeta(x));
	checkEquals(NULL, colmeta(x, use.colnames=TRUE));
		
	dimmeta(x) <- list(NULL, NULL);
	checkEquals(list(NULL, NULL), dimmeta(x, use.dimnames=TRUE));
	checkEquals(NULL, rowmeta(x, use.rownames=TRUE));
	checkEquals(NULL, rowmeta(x, use.rownames=FALSE));
	checkEquals(NULL, colmeta(x, use.colnames=FALSE));
	checkEquals(NULL, colmeta(x, use.colnames=TRUE));
	
	dimmeta(x) <- NULL;
	checkEquals(NULL, dimmeta(x, use.dimnames=TRUE));
	checkEquals(NULL, rowmeta(x, use.rownames=TRUE));
	checkEquals(NULL, rowmeta(x, use.rownames=FALSE));
	checkEquals(NULL, colmeta(x, use.colnames=FALSE));
	checkEquals(NULL, colmeta(x, use.colnames=TRUE));	
})

test_replace_dimmeta <- svTest(function() {
	x <- marray(1:6, dim=c(2, 3), dimmeta=list(letters[1:2], LETTERS[1:3]));
	
	checkEquals(list(letters[1:2], LETTERS[1:3]), dimmeta(x));

	dimmeta(x) <- list(c("r", "j"), c("X1", "X2", "X3"))
	checkEquals(list(c("r", "j"), c("X1", "X2", "X3")), dimmeta(x));

	dimmeta(x)[[1L]] <- c("r1", "j1");
	checkEquals(list(c("r1", "j1"), c("X1", "X2", "X3")), dimmeta(x));
	
	dimmeta(x) <- list(c("a", "b"))
	checkEquals(list(c("a", "b"), NULL), dimmeta(x));
	
	dimmeta(x) <- NULL;
	checkEquals(NULL, dimmeta(x));
	checkEquals("marray", class(x));
	
	checkException(dimmeta(x) <- letters[1:6], "value must be a list");
	checkException(dimmeta(x) <- list(letters[1:2], LETTERS[1:3], 1:2), 
		"number of dimensions must be lower or equal");
	checkException(dimmeta(x) <- list(letters[1], LETTERS[1:4]), 
		"lengths for each dimension must match");
})

test_rbind <- svTest(function() {
	# With vectors --elements without metadata become NA
	x <- marray(1:6, dim=c(2, 3), dimmeta=list(letters[1:2], LETTERS[1:3]));
	y <- marray(10, dim=c(1, 3), dimmeta=list("j", c("X1", "X2", "X3")));
	
	checkEquals(marray(c(1, 2, 10, 3, 4, 10, 5, 6, 10), dim=c(3, 3), 
			dimmeta=list(c("a", "b", "j"), LETTERS[1:3])),
		rbind(x, y));

	checkEquals(marray(c(1, 1, 2, 1, 3, 4, 1, 5, 6), dim=c(3, 3), 
			dimmeta=list(c(NA_character_, letters[1:2]), LETTERS[1:3])),
		rbind(1, x));
		
	# With lists --elements without metadata become list(NULL)
	xl <- marray(1:6, dim=c(2, 3), 
		dimmeta=list(list("a", "b"), list("A", "B", "C")));

	checkEquals(marray(c(1, 2, 1, 2, 3, 4, 3, 4, 5, 6, 5, 6), dim=c(4, 3), 
			dimmeta=list(list("a", "b", "a", "b"), list("A", "B", "C"))),
		rbind(xl, xl));

	checkEquals(marray(c(1, 1, 2, 1, 3, 4, 1, 5, 6), dim=c(3, 3), 
			dimmeta=list(list(NULL, "a", "b"), list("A", "B", "C"))),
		rbind(1, xl));
	
	# Join lists and vectors --they are coerced to list
	checkEquals(marray(c(1, 2, 2, 1, 2, 3, 4, 2, 3, 4, 5, 6, 2, 5, 6), 
			dim=c(5, 3), 
			dimmeta=list(list("a", "b", NULL, "a", "b"), list("A", "B", "C"))),
		rbind(xl, 2, x));	
		
})

test_cbind <- svTest(function() {
	# With vectors --elements without metadata become NA
	x <- marray(1:6, dim=c(2, 3), dimmeta=list(letters[1:2], LETTERS[1:3]));
	y <- marray(10, dim=c(2, 1), dimmeta=list(c("j", "w"), "X1"));
	
	checkEquals(marray(c(1, 2, 3, 4, 5, 6, 10, 10), dim=c(2, 4), 
			dimmeta=list(c("a", "b"), c("A", "B", "C", "X1"))),
		cbind(x, y));

	checkEquals(marray(c(1, 1, 1:6), dim=c(2, 4), 
			dimmeta=list(letters[1:2], c(NA_character_, "A", "B", "C"))),
		cbind(1, x));
		
	# With lists --elements without metadata become list(NULL)
	xl <- marray(1:4, dim=c(2, 2), 
		dimmeta=list(list("w", "j"), list("X1", "X2")));

	checkEquals(marray(c(1:4, 1:4), dim=c(2, 4), 
			dimmeta=list(list("w", "j"), list("X1", "X2", "X1", "X2"))),
		cbind(xl, xl));

	checkEquals(marray(c(1:4, -10, -20), dim=c(2, 3), 
			dimmeta=list(list("w", "j"), list("X1", "X2", NULL))),
		cbind(xl, c(-10, -20)));
	
	# Join lists and vectors --they are coerced to list
	checkEquals(marray(c(1:4, 2, 2, 1:6), 
			dim=c(2, 6), 
			dimmeta=list(list("w", "j"), list("X1", "X2", NULL, "A", "B", "C"))),
		cbind(xl, 2, x));	
		
})