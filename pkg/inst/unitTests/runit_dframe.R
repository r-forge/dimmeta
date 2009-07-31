################################################################################
# R PACKAGE:   dimmeta
# FILE:        R/runit_dframe.R
# DESCRIPTION: Test suite for the 'dframe' S3 class. To be run with svUnit. 
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  2009 July 24
# LICENSE:     GPL-2
################################################################################

test_dframe_withoutDimattr <- svTest(function() {		
	x <- dframe(X=1:2, Y=c("eur", "usd"), Z=I(c("A", "B")));
	
	checkTrue(inherits(x, "dframe"));
	checkTrue(inherits(x, "data.frame"));
	checkTrue(is.dframe(x));
	checkTrue(is.data.frame(x));
	
	checkEquals(c(2L, 3L), dim(x));
	checkEquals(list(c("1", "2"), c("X", "Y", "Z")), dimnames(x));
	checkEquals(NULL, dimmeta(x));
})

test_simple_dframe <- svTest(function() {	
	xda <- list(letters[1:2], LETTERS[1:3]);
	x <- dframe(X=1:2, Y=c("eur", "usd"), Z=I(c("A", "B")), dimmeta=xda);
	
	checkTrue(is.data.frame(x));
	checkTrue(is.dframe(x));
	
	checkEquals(xda, dimmeta(x));
})

test_extract_colsOnly <- svTest(function() {
	xda <- list(letters[1:2], LETTERS[1:3]);
	x <- dframe(X=1:2, Y=c("eur", "usd"), Z=I(c("A", "B")), dimmeta=xda);
	
	# Using a single argument returns a data frame, irrespective
	# of the value of 'drop'
	checkEquals(dframe(Y=c("eur", "usd"), dimmeta=list(c("a", "b"), "B")), 
		suppressWarnings(x["Y",drop=FALSE]), 
		"only one character column subscript with drop=FALSE");
	
	checkEquals(dframe(Y=c("eur", "usd"), dimmeta=list(c("a", "b"), "B")), 
		suppressWarnings(x[c(FALSE, TRUE, FALSE),drop=TRUE]),
		"only one logical column subscript with drop=TRUE");

	checkEquals(dframe(X=1:2, Z=I(c("A", "B")), 
		dimmeta=list(c("a", "b"), c("A", "C"))), x[-2],
		"only one negative integer column subscript");
})

test_extract_rowsOnly <- svTest(function() {
	xda <- list(letters[1:2], LETTERS[1:3]);
	x <- dframe(X=1:2, Y=c("eur", "usd"), Z=I(c("A", "B")), 
		row.names=c("r1", "r2"), dimmeta=xda);
	
	checkEquals(dframe(X=2, Y=factor("usd", levels=c("eur", "usd")), Z=I("B"),
		row.names="r2", dimmeta=list("b", c("A", "B", "C"))), 
		x["r2",,drop=FALSE], 
		"subset one row using a character vector");

	# With drop=TRUE, returns a list instead of a data.frame. dimmeta 
	# attribute is kept and subsetted.
	checkEquals(structure(
			list(X=2, Y=factor("usd", levels=c("eur", "usd")), Z=I("B")),
			dimmeta=list("b", c("A", "B", "C"))), x[-1,,drop=TRUE],
		"with drop=TRUE returns a list with dimmeta attributes");

	y <- x[2:3,];
	rownames(y) <- NULL;
	checkEquals(dframe(X=c(2, NA_real_),  
		Y=factor(c("usd", NA), levels=c("eur", "usd")), 
		Z=I(c("B", NA_character_)), dimmeta=list(c("b", NA), c("A", "B", "C"))),
		y, "out-of-range rows produce NAs in the data and the dimmeta");

})

test_extract_rowsAndCols <- svTest(function() {
	xda <- list(letters[1:2], LETTERS[1:3]);
	x <- dframe(X=1:2, Y=c("eur", "usd"), Z=I(c("A", "B")), dimmeta=xda);
	
	checkEquals(x[2:1, 3:2, drop=FALSE], x[2:1, 3:2, drop=TRUE]);
	
	y <- x[2:1, c("Z", "Y")];
	rownames(y) <- NULL;
	checkEquals(dframe(Z=I(c("B", "A")), 
		Y=factor(c("usd", "eur"), levels=c("eur", "usd")),
		dimmeta=list(c("b", "a"), c("C", "B"))),
		y, "subscript both rows and columns without dropping")
	
	# Note we don't care about columns out-of-range: they produce NULLs if 
	# drop=TRUE (data frame is dropped), and errors if drop=FALSE
	# TODO: Test for metadata being vectors, lists, arrays and dfs!
	y <- x[3:2, "Y", drop=FALSE];
	rownames(y) <- NULL;
	checkEquals(dframe(Y=factor(c(NA, "usd"), levels=c("eur", "usd")),
		dimmeta=list(c(NA, "b"), "B")), y,
		"out-of-range rows and one column subscript with drop=FALSE");						
})

test_extract_indexEdgeCases <- svTest(function() {
	xda <- list(letters[1:2], data.frame(labels=I(c("isX", "isY", "isZ"))));
	x <- dframe(X=1:2, Y=c("eur", "usd"), Z=I(c("A", "B")), dimmeta=xda);
	
	checkEquals(dframe(
			dimmeta=list(character(0), data.frame(labels=I(character(0))))),
		x[NULL, NULL]);

	checkEquals(dframe(
			dimmeta=list(character(0), data.frame(labels=I(character(0))))),
		x[integer(0), character(0)]);

	checkEquals(dframe(
			dimmeta=list(character(0), data.frame(labels=I(character(0))))),
		x[0, 0]);

	y <- x[0];
	row.names(y) <- c("1", "2");
	checkEquals(dframe(row.names=c("1", "2"),
			dimmeta=list(letters[1:2], data.frame(labels=I(character(0))))),
		y);
	
	y <- x[NA,,drop=FALSE];
	checkEquals(
		data.frame(X=1:2, Y=c("eur", "usd"), Z=I(c("A", "B")))[NA,,drop=FALSE],
		as.data.frame(y));
	checkEquals(list(letters[1:2][NA], xda[[2L]]),
		dimmeta(y));	

	checkException(x[NA]);
	checkException(x[,NA]);
})

test_extract_dimmetaList <- svTest(function() {
	x <- dframe(X=1:2, Y=c("eur", "usd"), Z=I(c("A", "B")), 
		dimmeta=list(list(a="r1", 3L), list("C1", b=c("C2.1", "C2.2"), "C3")));
	
	checkEquals(dframe(Y=factor("eur", levels=c("eur", "usd")), 
		dimmeta=list(list(a="r1"), list(b=c("C2.1", "C2.2")))),
		x[c(TRUE, FALSE), c(-1, -3), drop=FALSE],
		"subset existing rows and columns with list dimmeta");

	# NOTE: List dimmeta when new elements are added due to out-of-range
	# rows have NAs as list names and NULLs as values. 
	# When the list is created directly without names, empty strings "" are 
	# used for empty names instead of NAs. That's why the commented test fails.
	y <- x[c(1, 3), c(FALSE, FALSE, TRUE), drop=FALSE];
	row.names(y) <- NULL;
#	checkEquals(dframe(Z=I(c("A", NA_character_)), 
#		dimmeta=list(list(a="r1", NULL), list("C3"))),
#		y, "subset out-of-bound rows and columns with list dimmeta");
	checkEquals(data.frame(Z=I(c("A", NA_character_))),
		as.data.frame(y));
	checkEquals(list("r1", NULL), dimmeta(y)[[1L]], 
		msg="extraction of out-of-bounds row index produce NULL in the dimmeta list",
		checkNames=FALSE)
})

test_extract_dimmetaArray <- svTest(function() {
	xda <- list(array(1:4, c(2, 1, 2)), array(10:22, c(2, 3, 2)));  
	x <- dframe(X=1:2, Z=I(c("A", "B")), row.names=c("r1", "r2"), dimmeta=xda);
	
	checkEquals(dframe(Z=I("B"), X=2, row.names="r2", 
		dimmeta=list(xda[[1L]][2,,,drop=FALSE], xda[[2L]][2:1,,,drop=FALSE])), 
		x[2, c("Z", "X")], "normal extraction");
	
	y <- x[c(1,5), c(FALSE, TRUE), drop=FALSE];
	row.names(y) <- NULL;	
	checkEquals(dframe(Z=I(c("A", NA_character_)), 
		dimmeta=list(array(c(1, NA, 3, NA), c(2, 1, 2)),  
			xda[[2L]][2,,,drop=FALSE])), y, 
		"extraction of out-of-bounds row index produce NAs in the dimmeta array");
})

test_extract_dimmetaDF <- svTest(function() {
	xda <- list(data.frame(X1=c(-1, -2), X2=factor(c("eur", "usd"))), 
		data.frame(labels=c("isX", "isY", "isZ")));
	x <- dframe(X=1:2, Y=c("eur", "usd"), Z=I(c("A", "B")), 
		dimmeta=xda);
	
	y <- x[2:1, 3, drop=FALSE];
	rownames(y) <- NULL;
	checkEquals(dframe(Z=I(c("B", "A")), 
		dimmeta=list(
			data.frame(X1=c(-1, -2), X2=factor(c("eur", "usd")))[
				c(2,1),,drop=FALSE], 
			data.frame(labels=c("isX", "isY", "isZ"))[3,,drop=FALSE])),	
		y, "use data frames as dimmeta");
 	
	y <- x[c(1,5), 1, drop=FALSE];
	row.names(y) <- NULL;
	checkEquals(dframe(X=c(1, NA_real_),					
			dimmeta=list(xda[[1L]][c(1,5),,drop=FALSE],
				xda[[2L]][1,,drop=FALSE])),
			y, "out-of-bounds rows with data frames as dimmeta");
})

test_extract_keepAttrs <- svTest(function() {
	xda <- list(letters[1:2], LETTERS[1:3]);
	x <- dframe(X=1:2, Y=c("eur", "usd"), Z=I(c("A", "B")), dimmeta=xda);
	attr(x, "test") <- 100L;
	attr(x, "more") <- "X";
	
	ya <- ya1 <- y <- dframe(X=1:2, dimmeta=list(letters[1:2], "A"));
	attr(ya, "test") <- 100L;
	attr(ya, "more") <- "X";
	attr(ya1, "test") <- 100L;
		
	checkEquals(y, x[1]);
	checkEquals(y, x[1, keep.attrs=FALSE]);
	checkEquals(ya, x[1, keep.attrs=TRUE]);
	checkEquals(ya1, x[1, keep.attrs="test"]);
	
	checkEquals(y, x[, 1, drop=FALSE]);
	checkEquals(y, x[, 1, keep.attrs=FALSE, drop=FALSE]);
	checkEquals(ya, x[, 1, keep.attrs=TRUE, drop=FALSE]);
	checkEquals(ya1, x[, 1, drop=FALSE, keep.attrs="test"]);
	
	checkEquals(y, x[1:2, 1, drop=FALSE]);
	checkEquals(y, x[1:2, 1, keep.attrs=FALSE, drop=FALSE]);
	checkEquals(ya, x[1:2, 1, keep.attrs=TRUE, drop=FALSE]);
	checkEquals(ya1, x[1:2, 1, drop=FALSE, keep.attrs="test"]);

	ya <- ya1 <- y <- x[2,,drop=FALSE];
	attr(ya, "test") <- 100L;
	attr(ya, "more") <- "X";
	attr(ya1, "test") <- 100L;
	
	checkEquals(y, x[2,]);
	checkEquals(y, x[2, , keep.attrs=FALSE, drop=FALSE]);
	checkEquals(ya, x[2, , keep.attrs=TRUE, drop=FALSE]);
	checkEquals(ya1, x[2, , drop=FALSE, keep.attrs="test"]);		
})

test_replace_dimNotModified <- svTest(function() {
	xda <- list(letters[1:2], LETTERS[1]);
	x <- dframe(X=1:2, dimmeta=xda);

	y <- x; y[c(FALSE, TRUE), 1] <- 3;
	checkEquals(dframe(X=c(1, 3), dimmeta=xda), y);
	
	y <- x; y[1] <- 4;
	checkEquals(dframe(X=c(4, 4), dimmeta=xda), y);

	y <- x; y[,TRUE] <- 5;
	checkEquals(dframe(X=c(5, 5), dimmeta=xda), y);

	y <- x; y[1,] <- 6;
	checkEquals(dframe(X=c(6, 2), dimmeta=xda), y);
	
	y <- x; y[] <- 7;
	checkEquals(dframe(X=c(7, 7), dimmeta=xda), y);
})

test_replace_indexEdgeCases <- svTest(function() {
	xda <- list(letters[1:2], LETTERS[1]);
	x <- dframe(X=1:2, dimmeta=xda);
		
	checkException(x[NA] <- 1);
	checkException(x[,NA] <- 1);	
	checkException(x[NA,] <- 1);
	checkException(x[NA, NA] <- 1);
	
	checkException(x[0L] <- 1);
	checkException(x[,0L] <- 1);	
	checkException(x[0L, 0L] <- 1);

	y <- x; y[0L,] <- 100; checkEquals(y, x);
	
	y <- x; y[NULL] <- 100; checkEquals(y, x);
	y <- x; y[,NULL] <- 100; checkEquals(y, x);
	y <- x; y[NULL,] <- 100; checkEquals(y, x);
	y <- x; y[NULL, NULL] <- 100; checkEquals(y, x);
	
	y <- x; y[integer(0)] <- 100; checkEquals(y, x);
	y <- x; y[,logical(0)] <- 100; checkEquals(y, x);
	y <- x; suppressWarnings(y[character(0),] <- 100); checkEquals(y, x);
	y <- x; suppressWarnings(y[logical(0), character(0)] <- 100); 
		checkEquals(y, x);		
})

test_replace_modifyDimmetaAtomicVector <- svTest(function() {
	xda <- list(letters[1:2], LETTERS[1]);
	x <- dframe(X=1:2, dimmeta=xda);

	y <- x;	y[4, 1] <- 4;
	checkEquals(dframe(X=c(1, 2, NA, 4), 
		dimmeta=list(c("a", "b", NA, NA), "A")), y,
		"add row with [i, j] syntax adds NA to rowmeta");
	
	y <- x; y["W"] <- 10;
	checkEquals(dframe(X=c(1, 2), W=c(10, 10), 
		dimmeta=list(c("a", "b"), c("A", NA))), y,
		"add colum with [i] syntax adds NA to colmeta");

	y <- x; y[,"W"] <- 10;
	checkEquals(dframe(X=c(1, 2), W=c(10, 10), 
		dimmeta=list(c("a", "b"), c("A", NA))), y,
		"add column with [,j] syntax adds NA to colmeta");

	y <- x; y[3, 2] <- 5;
	checkEquals(dframe(X=c(1, 2, NA), V2=c(NA, NA, 5), 
		dimmeta=list(c("a", "b", NA), c("A", NA))), y,
		"add row and column with [i, j] syntax add NAs to row & colmeta");
	
	y <- x; y[1] <- NULL;
	checkEquals(dframe(row.names=1:2, dimmeta=list(c("a", "b"), character(0))),
    	y, "remove row with [i]<-NULL syntax also subsets rowmeta") 

	y <- x; y[,1] <- NULL;
	checkEquals(dframe(row.names=1:2, dimmeta=list(c("a", "b"), character(0))), 
		y, "remove row with [,j]<-NULL syntax also subsets colmeta");		
})

test_replace_modifyDimmetaList <- svTest(function() {
	xda <- list(list("a", "b"), list(1:2));
	x <- dframe(X=1:2, dimmeta=xda);

	y <- x;	y[4, 1] <- 4;
	checkEquals(dframe(X=c(1, 2, NA, 4), 
		dimmeta=list(list("a", "b", NULL, NULL), list(1:2))), y,
		"add row with [i, j] syntax adds NULL to rowmeta");
	
	y <- x; y["W"] <- 10;
	checkEquals(dframe(X=c(1, 2), W=c(10, 10), 
		dimmeta=list(list("a", "b"), list(1:2, NULL))), y,
		"add colum with [i] syntax adds NULL to colmeta");

	y <- x; y[,"W"] <- 10;
	checkEquals(dframe(X=c(1, 2), W=c(10, 10), 
		dimmeta=list(list("a", "b"), list(1:2, NULL))), y,
		"add column with [,j] syntax adds NULL to colmeta");

	y <- x; y[3, 2] <- 5;
	checkEquals(dframe(X=c(1, 2, NA), V2=c(NA, NA, 5), 
		dimmeta=list(list("a", "b", NULL), list(1:2, NULL))), y,
		"add row and column with [i, j] syntax add NULLs to row & colmeta");
	
	y <- x; y[1] <- NULL;
	checkEquals(dframe(row.names=1:2, dimmeta=list(list("a", "b"), list())),
    	y, "remove row with [i]<-NULL syntax also subsets rowmeta") 

	y <- x; y[,1] <- NULL;
	checkEquals(dframe(row.names=1:2, dimmeta=list(list("a", "b"), list())), 
		y, "remove row with [,j]<-NULL syntax also subsets colmeta");		
})

test_replace_modifyDimmetaArray <- svTest(function() {
	xda <- list(array(1:4, c(2, 1, 2)), array(10:11, c(1, 2, 1)));
	x <- dframe(X=1:2, dimmeta=xda);

	y <- x;	y[4, 1] <- 4;
	checkEquals(dframe(X=c(1, 2, NA, 4), 
		dimmeta=list(array(c(1,2,NA,NA,3,4,NA,NA), c(4, 1, 2)), xda[[2L]])), y,
		"add row with [i, j] syntax adds rows to rowmeta");
	
	y <- x; y["W"] <- 10;
	checkEquals(dframe(X=c(1, 2), W=c(10, 10), 
		dimmeta=list(xda[[1L]], array(c(10, NA, 11, NA), c(2, 2, 1)))), y,
		"add colum with [i] syntax adds rows to colmeta");

	y <- x; y[,"W"] <- 10;
	checkEquals(dframe(X=c(1, 2), W=c(10, 10), 
		dimmeta=list(xda[[1L]], array(c(10, NA, 11, NA), c(2, 2, 1)))), y,
		"add column with [,j] syntax adds rows to colmeta");

	y <- x; y[3, 2] <- 5;
	checkEquals(dframe(X=c(1, 2, NA), V2=c(NA, NA, 5), 
		dimmeta=list(array(c(1,2,NA,3,4,NA), c(3, 1, 2)), 
			array(c(10, NA, 11, NA), c(2, 2, 1)))), y,
		"add row and column with [i, j] syntax add rows to row & colmeta");
	
	y <- x; y[1] <- NULL;
	checkEquals(dframe(row.names=1:2, 
			dimmeta=list(xda[[1L]], array(numeric(0), dim=c(0,2,1)))),
    	y, "remove row with [i]<-NULL syntax also subsets rowmeta") 

	y <- x; y[,1] <- NULL;
	checkEquals(dframe(row.names=1:2, 
			dimmeta=list(xda[[1L]], array(numeric(0), dim=c(0,2,1)))), 
		y, "remove row with [,j]<-NULL syntax also subsets colmeta");		
		
})

test_replace_modifyDimmetaDF <- svTest(function() {
	xda <- list(data.frame(A=-1:-2, B=I(c("x", "y"))), 
		data.frame(labels=I("fc"), units=I("m")));
	x <- dframe(X=1:2, dimmeta=xda);

	y <- x;	y[4, 1] <- 4;
	checkEquals(dframe(X=c(1, 2, NA, 4), 
		dimmeta=list(data.frame(A=c(-1,-2,NA,NA), B=I(c("x", "y", NA, NA))),
			xda[[2L]])), y,
		"add row with [i, j] syntax adds rows to rowmeta");
	
	y <- x; y["W"] <- 10;
	checkEquals(dframe(X=c(1, 2), W=c(10, 10), 
		dimmeta=list(xda[[1L]], 
			data.frame(labels=I(c("fc", NA)), units=I(c("m", NA))))), y,
		"add colum with [i] syntax adds rows to colmeta");

	y <- x; y[,"W"] <- 10;
	checkEquals(dframe(X=c(1, 2), W=c(10, 10), 
		dimmeta=list(xda[[1L]], 
			data.frame(labels=I(c("fc", NA)), units=I(c("m", NA))))), y,
		"add column with [,j] syntax adds rows to colmeta");

	y <- x; y[3, 2] <- 5;
	checkEquals(dframe(X=c(1, 2, NA), V2=c(NA, NA, 5), 
		dimmeta=list(
			data.frame(A=c(-1,-2,NA), B=I(c("x", "y", NA))), 
			data.frame(labels=I(c("fc", NA)), units=I(c("m", NA))))), y,
		"add row and column with [i, j] syntax add rows to row & colmeta");
	
	y <- x; y[1] <- NULL;
	checkEquals(dframe(row.names=1:2, 
			dimmeta=list(xda[[1L]], 
			data.frame(labels=I(character(0)), units=I(character(0))))),
    	y, "remove row with [i]<-NULL syntax also subsets rowmeta") 

	y <- x; y[,1] <- NULL;
	checkEquals(dframe(row.names=1:2, 
			dimmeta=list(xda[[1L]], 
			data.frame(labels=I(character(0)), units=I(character(0))))),
		y, "remove row with [,j]<-NULL syntax also subsets colmeta");		
		
})
