################################################################################
# R PACKAGE:   dimmeta
# FILE:        R/runit_darray.R
# DESCRIPTION: Test suite for the 'darray' S3 class. To be run with svUnit. 
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  2009 July 7
# LICENSE:     GPL-2
################################################################################

#y <- as.array(x)
#dimnames(y) <- list(paste("r", 1:2, sep=""), paste("c", 1:3, sep="")) 

test_darray_withoutDimattr <- svTest(function() {		
	x <- darray(1:6, dim=c(2, 3))
	
	checkTrue(inherits(x, "darray"));
	checkTrue(is.array(x));
	checkTrue(is.darray(x));
	
	checkEquals(c(2L, 3L), dim(x));
	checkEquals(NULL, dimnames(x));
	checkEquals(NULL, dimmeta(x));
})

test_simple_darray <- svTest(function() {	
	xda <- list(letters[1:2], LETTERS[1:3]);
	x <- darray(1:6, dim=c(2, 3), dimmeta=xda);
	
	checkTrue(inherits(x, "darray"));
	checkTrue(is.array(x));
	checkTrue(is.darray(x));
	
	checkEquals(xda, dimmeta(x));
})

# If 'dim' is NOT preseved, 'dimmeta' isn't, neither.
# Note that when using more than one index and drop=TRUE and a vector
#	is returned, 'dimnames' is transformed to 'names'. What should we
#	do here? A 1-d array? A new class for 'vector'? A standard 'vector'
#   with an extra attribute?

test_extract_2d_dropDim <- svTest(function() {
	xda <- list(letters[1:2], LETTERS[1:3]);
	x <- darray(1:6, dim=c(2, 3), dimmeta=xda);
	
	# Using a single argument indexes as a vector, irrespective of 'drop' value 
	checkEquals(2, x[2,drop=FALSE]);
	checkEquals(2, x[2,drop=TRUE]);

	# With drop=TRUE and returning a vector, dimmeta is dropped		
	checkEquals(c(5, 6), x[,3]);	
	checkEquals(c(1, 3, 5), x[1,]);
	checkEquals(c(6, 2), x[2, c(3, 1)]);
	
	# Reduce the number of dimensions
	x <- darray(1:8, dim=c(2, 2, 2), 
		dimmeta=list(letters[1:2], LETTERS[1:2], 1:2));
 
	checkEquals(darray(5:8, dim=c(2, 2), 
			dimmeta=list(letters[1:2], LETTERS[1:2])),
		x[,,2]);
})

test_extract_2d_keepDim <- svTest(function() {
	xda <- list(letters[1:2], LETTERS[1:3]);
	x <- darray(1:6, dim=c(2, 3), dimmeta=xda);
	
	checkEquals(x, x[], "no index returns the darray unchanged");
    	
	checkEquals(darray(6, dim=c(1, 1), dimmeta=list("b", "C")), 
		x[c(FALSE, TRUE), c(FALSE, FALSE, TRUE), drop=FALSE],
		"With drop=FALSE and 2 indexes, keep dimmeta");
	
	checkEquals(darray(c(2, 1, 4, 3, 6, 5), dim=c(2, 3), 
			dimmeta=list(c("b", "a"), c("A", "B", "C"))), 
		x[2:1,],
		"With drop=FALSE and one index missing, keep dimmeta");	
		
	checkEquals(darray(c(6, 5, 2, 1), dim=c(2, 2), 
			dimmeta=list(c("b", "a"), c("C", "A"))), 
		x[2:1, c(3, 1)],
		"select an array with one column less, resorting rows and columns");
	
	dimnames(x) <- list(paste("r", 1:2, sep=""), paste("c", 1:3, sep="")); 
	checkEquals(darray(c(4, 3, 2, 1), dim=c(2, 2),
    		dimnames=list(c("r2", "r1"), c("c2", "c1")),
			dimmeta=list(c("b", "a"), c("B", "A"))), 
		x[c("r2", "r1"), c("c2", "c1")],
		"extract using names (character vector subscript)");
	
	checkEquals(c(6, 6, 3), x[matrix(c(2, 2, 1, 3, 3, 2), 3, 2)],
		"extract using matrix always drops dimensions, coercing to vector");
})

test_extract_2d_indexEdgeCases <- svTest(function() {
	xda <- list(letters[1:2], LETTERS[1:3]);
	x <- darray(1:6, dim=c(2, 3), dimmeta=xda);
	
	checkEquals(darray(integer(0), dim=c(0,0), 
			dimmeta=list(character(0), character(0))),
		x[NULL, NULL]);

	checkEquals(darray(integer(0), dim=c(0,0), 
			dimmeta=list(character(0), character(0))),
		x[integer(0), logical(0)]);

	checkEquals(darray(integer(0), dim=c(0,0), 
			dimmeta=list(character(0), character(0))),
		x[0, 0]);

	checkEquals(darray(integer(0), dim=c(2,0), 
			dimmeta=list(c("a", "b"), character(0))),
		x[, 0]);
	
	checkEquals(darray(NA_integer_, dim=c(2, 3), 
			dimmeta=list(rep(NA_character_, 2), c("A", "B", "C"))),
		x[NA, ]);	
})

test_extract_2d_dimmetaDF <- svTest(function() {
	xda <- list(data.frame(X1=c(-1, -2), X2=factor(c("eur", "usd"))), 
		list(X=1, Y=5:3, Z=list("wow!", "ja!")));
	x <- darray(1:6, dim=c(2, 3), dimmeta=xda);
	
	checkEquals(darray(c(6, 5), dim=c(2, 1), 
			dimmeta=list(
				data.frame(X1=c(-1, -2), X2=factor(c("eur", "usd")))[
					c(2,1),,drop=FALSE], 
				list(Z=list("wow!", "ja!"))
			)),		
		x[2:1, 3, drop=FALSE],
		"use data frame and list with each item a different lengths as dimmeta");
 	
	checkException(darray(1:6, dim=c(2, 3), dimmeta=list(
			data.frame(X1=-1:-3, X2=factor(c("eur", "usd", "eur"))), 
			list(X=1, Y=5:3, Z=list("wow!", "ja!"))
		)), 
		"number of rows of data frame does not equal number of columns in array");
})

test_extract_keepAttrs <- svTest(function() {
	xda <- list(letters[1:2], LETTERS[1:3]);
	x <- darray(1:6, dim=c(2, 3), dimmeta=xda);
	xa <- x;
	attr(xa, "label") <- "test";
	attr(xa, "other") <- 10;
	
	checkEquals(darray(c(1,2), c(2,1), dimmeta=list(letters[1:2], LETTERS[1])),
		xa[,1,drop=FALSE]);	
	checkEquals(darray(c(1,2), c(2,1), dimmeta=list(letters[1:2], LETTERS[1])),
		xa[,1,drop=FALSE,keep.attrs=FALSE]);
	
	checkEquals(structure(
			darray(c(1,2), c(2,1), dimmeta=list(letters[1:2], LETTERS[1])),
			label="test", other=10),
		xa[,1,drop=FALSE,keep.attrs=TRUE]);

	checkEquals(structure(
			darray(c(1,2), c(2,1), dimmeta=list(letters[1:2], LETTERS[1])),
			other=10),
		xa[,1,drop=FALSE,keep.attrs="other"]);	
})
		
test_extract_dimmeta <- svTest(function() {
	xda <- list(data.frame(A1=-1:-2, A2=factor(c("eur", "usd"))), 
		list(X=1, Y=5:3, Z=list("wow!", "ja!")));
	x <- darray(1:6, dim=c(2, 3), dimmeta=xda,
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
	x <- darray(1:6, dim=c(2, 3), 
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
	x <- darray(1:6, dim=c(2, 3), dimmeta=list(letters[1:2], LETTERS[1:3]));
	
	checkEquals(list(letters[1:2], LETTERS[1:3]), dimmeta(x));

	dimmeta(x) <- list(c("r", "j"), c("X1", "X2", "X3"))
	checkEquals(list(c("r", "j"), c("X1", "X2", "X3")), dimmeta(x));

	dimmeta(x)[[1L]] <- c("r1", "j1");
	checkEquals(list(c("r1", "j1"), c("X1", "X2", "X3")), dimmeta(x));
	
	dimmeta(x) <- list(c("a", "b"))
	checkEquals(list(c("a", "b"), NULL), dimmeta(x));
	
	dimmeta(x) <- NULL;
	checkEquals(NULL, dimmeta(x));
	checkEquals("darray", class(x));
	
	checkException(dimmeta(x) <- letters[1:6], "value must be a list");
	checkException(dimmeta(x) <- list(letters[1:2], LETTERS[1:3], 1:2), 
		"number of dimensions must be lower or equal");
	checkException(dimmeta(x) <- list(letters[1], LETTERS[1:4]), 
		"lengths for each dimension must match");
})

test_rbind_vectorMeta <- svTest(function() {
	x <- darray(1:6, dim=c(2, 3), dimmeta=list(letters[1:2], LETTERS[1:3]));
	y <- darray(10, dim=c(1, 3), dimmeta=list("j", c("X1", "X2", "X3")));

	checkEquals(rbind(x, y), rbind(integer(0), x, NULL, y),
		"elements with zero-length are ignored by rbind");		
	
	checkEquals(darray(c(1, 2, 10, 3, 4, 10, 5, 6, 10), dim=c(3, 3), 
			dimmeta=list(c("a", "b", "j"), LETTERS[1:3])),
		rbind(x, y),
		"rbind atomic vector dimmeta with all elements");

	checkEquals(darray(c(1, 1, 2, 1, 3, 4, 1, 5, 6), dim=c(3, 3), 
			dimmeta=list(c(NA_character_, letters[1:2]), LETTERS[1:3])),
		rbind(1, x),
		"rbind atomic vector dimmeta with objects without dimmeta produces NAs");
		
	xl <- darray(1:6, dim=c(2, 3), 
		dimmeta=list(list("a", "b"), list("A", "B", "C")));
	checkEquals(darray(c(1, 2, 1, 2, 3, 4, 3, 4, 5, 6, 5, 6), dim=c(4, 3), 
			dimmeta=list(list("a", "b", "a", "b"), list("A", "B", "C"))),
		rbind(xl, xl),
		"rbind list vector dimmeta with all elements");

	checkEquals(darray(c(1, 1, 2, 1, 3, 4, 1, 5, 6), dim=c(3, 3), 
			dimmeta=list(list(NULL, "a", "b"), list("A", "B", "C"))),
		rbind(1, xl),
		"rbind list vector dimmeta with objects without dimmeta produces list(NULL)");
	
	checkEquals(darray(c(1, 2, 2, 1, 2, 3, 4, 2, 3, 4, 5, 6, 2, 5, 6), 
			dim=c(5, 3), 
			dimmeta=list(list("a", "b", NULL, "a", "b"), list("A", "B", "C"))),
		rbind(xl, 2, x),
		"rbinding list and atomic vectors dimmeta coerces all to list");	
})

test_rbind_dfMeta <- svTest(function() {
	x <- darray(1:6, dim=c(2, 3), dimmeta=list(
			data.frame(labels=I(c("r1", "r2")), units=c("m", "cm")),
			LETTERS[1:3]));	

	y <- darray(1:6, dim=c(1, 3), 
			dimmeta=list(data.frame(labels=I(c("R3")), units="m"), 
			c("Y1", "Y2", "Y3")));
	checkEquals(list(data.frame(
			labels=I(c("r1", "r2", "R3")), units=c("m", "cm", "m")),
			LETTERS[1:3]),
		dimmeta(rbind(x, y)),
		"rbind darrays with data frame dimmeta does rbind the data frames");	

	# The rules for rbinding the data frames on the dimmeta are followed
	dimmeta(y) <- list(data.frame(newLabels=I(c("R3")), newUnits="m"), 
			c("Y1", "Y2", "Y3"));
	checkException(rbind(x, y), 
		"variable names must match when combining the dimmeta data frames")

	checkEquals(darray(c(1, 1, 2, 1, 3, 4, 1, 5, 6), dim=c(3, 3), 
			dimmeta=list(
				data.frame(labels=I(c(NA_character_, "r1", "r2")), 
					units=c(NA, "m", "cm")),
				LETTERS[1:3])),
		rbind(1, x), 
		"rbind data frame dimmeta with objects without dimmeta produces NAs");

	dimmeta(y) <- list(c("R4"), c("Y1", "Y2", "Y3"));	
	checkEquals(darray(c(1, 2, 2, 1, 3, 4, 2, 2, 5, 6, 2, 3), dim=c(4, 3), 
			dimmeta=list(
				data.frame(labels=I(c("r1", "r2", NA_character_, "R4")), 
					units=c("m", "cm", NA, NA)),
				LETTERS[1:3])), 
		suppressWarnings(rbind(x, 2, y)),
		"rbind data frame with vector dimmeta does rbind them");	
});

test_rbind_arrayMeta <- svTest(function() {
	x <- darray(1:6, dim=c(2, 3), 
		dimmeta=list(matrix(10:13, 2, 2), matrix(letters[1:6], 3, 2)));	
	
	checkEquals(darray(c(1, 2, 10, 1, 2, 3, 4, 15, 3, 4, 5, 6, 20, 5, 6), 
			dim=c(5, 3), 
			dimmeta=list(
				rbind(matrix(10:13, 2, 2), c(NA_real_, NA_real_), 
					matrix(10:13, 2, 2)),
    			matrix(letters[1:6], 3, 2))),
		rbind(x, c(10, 15, 20), x),
		"rbind several arrays dimmeta with a plain vector");

	d1 <- matrix(10:13, 2, 2, dimnames=list(NULL, c("labels", "units")));
	d2 <- data.frame(labels=I(c("R3")), units="m");
	x <- darray(1:6, dim=c(2, 3), dimmeta=list(d1, matrix(letters[1:6], 3, 2)));		
	y <- darray(1:3, dim=c(1, 3), dimmeta=list(d2, c("Y1", "Y2", "Y3")));
	checkEquals(darray(c(1, 2, 1, 3, 4, 2, 5, 6, 3), dim=c(3, 3),
			dimmeta=list(rbind(d1, d2), c("Y1", "Y2", "Y3"))), 
		rbind(x, y),
		"rbind array & data frame dimmeta does result in a data frame");
})

test_cbind <- svTest(function() {
	# With vectors --elements without metadata become NA
	x <- darray(1:6, dim=c(2, 3), dimmeta=list(letters[1:2], LETTERS[1:3]));
	y <- darray(10, dim=c(2, 1), dimmeta=list(c("j", "w"), "X1"));
	
	checkEquals(darray(c(1, 2, 3, 4, 5, 6, 10, 10), dim=c(2, 4), 
			dimmeta=list(c("a", "b"), c("A", "B", "C", "X1"))),
		cbind(x, y));

	checkEquals(darray(c(1, 1, 1:6), dim=c(2, 4), 
			dimmeta=list(letters[1:2], c(NA_character_, "A", "B", "C"))),
		cbind(1, x));
		
	# With lists --elements without metadata become list(NULL)
	xl <- darray(1:4, dim=c(2, 2), 
		dimmeta=list(list("w", "j"), list("X1", "X2")));

	checkEquals(darray(c(1:4, 1:4), dim=c(2, 4), 
			dimmeta=list(list("w", "j"), list("X1", "X2", "X1", "X2"))),
		cbind(xl, xl));

	checkEquals(darray(c(1:4, -10, -20), dim=c(2, 3), 
			dimmeta=list(list("w", "j"), list("X1", "X2", NULL))),
		cbind(xl, c(-10, -20)));
	
	# Join lists and vectors --they are coerced to list
	checkEquals(darray(c(1:4, 2, 2, 1:6), 
			dim=c(2, 6), 
			dimmeta=list(list("w", "j"), list("X1", "X2", NULL, "A", "B", "C"))),
		cbind(xl, 2, x));	
		
})
