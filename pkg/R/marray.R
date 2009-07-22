################################################################################
# R PACKAGE:   metadata
# FILE:        R/marray.R
# DESCRIPTION: S3 class 'marray' 
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  2009 July 7
# LICENSE:     GPL-2
################################################################################

# Dev notes about arrays/matrices in R and subscripting
#  http://tolstoy.newcastle.edu.au/R/devel/05/01/1905.html
#  http://tolstoy.newcastle.edu.au/R/e2/devel/07/01/1950.html
#
#> Can the suscript operator be overloaded like that in R? (I know it can be in
#> S, at least for vectors.)
#> 
#
#Yes, there are many examples, see the Matrix package for some that use 
#new-style classes (in language issues like this, R is S, the differences 
#are in scoping).
#
#https://stat.ethz.ch/pipermail/bioc-devel/2009-June/001903.html 
#		(bioconductor - AnnotatedDataFrames)
# From http://tolstoy.newcastle.edu.au/R/announce/01c/0019.html:
#   C-Level facilities for R 1.4.0:
#    o arraySubscript and vectorSubscript are now available to package
#        users. All "array-like" packages can use a standard method for
#        calculating subscripts. 
#
# http://tolstoy.newcastle.edu.au/R/devel/05/11/3266.html


#' @export

marray <- function (data = NA, dim = length(data), dimnames = NULL,
		dimdata = NULL) {
	data <- array(data=data, dim=dim, dimnames=dimnames);
	if (length(dimdata))
		dimdata(data) <- dimdata;
	class(data) <- "marray";
	data;
}

#' @nord
#' @export

is.marray <- function(x) 
	is.array(x) && inherits(x, "marray");

#' @nord
#' @S3method as.marray default

as.marray.default <- function(x, dimdata = NULL, ...) { 
	x <- as.array(x);
	if (length(dimdata))
		dimdata(x) <- dimdata;
	class(x) <- "marray";
	x;
}

as.marray.marray <- function(x, ...) 
	x;

#' @nord
#' @export

as.array.marray <- function(x, ...) { 
	attr(x, "dimdata") <- NULL;
	class(x) <- NULL;
	x;
}

print.marray <- function(x, ...) {
	# This title is used for easier debugging. Probably not needed on
	# release version...
    cat("[array with dimensional attributes]\n");
	y <- x;
	x <- unclass(x);
	attr(x, "dimdata") <- NULL;
	NextMethod();
	invisible(y);
}

