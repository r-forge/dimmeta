################################################################################
# R PACKAGE:   metadata
# FILE:        R/marray.R
# DESCRIPTION: S3 class 'marray' 
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  2009 July 7
# LICENSE:     GPL-2
################################################################################

#' Creates or tests for arrays with metadata along their dimensions.
#' 
#' \code{marray} is an S3 class that provides infrastructure to add metadata 
#' to each dimension extent of an array. This metadata is stored in 
#' attribute \code{dimdata}, and its behaviour closely matches that of 
#' \code{dimnames}. 
#' 
#' Each component of \code{dimdata} must be an object whose length equals
#' the length of the array for the corresponding dimension and that can
#' be subscripted with the \code{[} operator: e.g. vectors, lists, or 
#' data frames.
#' 
#' Metadata along the dimensions is subsetted together with the array, 
#' exactly as \code{dimname}s. It is lost when dimensions are dropped on
#' subsetting operations.    
#' 
#' \section{Methods}{
#' \link[base:UseMethod]{Methods} to \link[base:InternalMethods]{standard 
#' generics} for "\code{marray}" objects currently include: 
#' \code{\link[print.marray]{print}}, the \code{\link[Extract.mframe]{[}}
#' extract and replace operator, \code{\link[rbind.marray]{rbind}} and 
#' \code{cbind}. These allow to keep the metadata along the dimensions
#' in sync with manipulations to the array data.
#'
#' New generic functions \code{\link{dimdata}}, \code{\link{rowdata}}
#' and \code{\link{coldata}} --in both setting and replacement versions--
#' allow to access the components of the metadata. The default implementations
#' get and set the \code{dimdata} attribute --irrespective of whether 
#' the object is of class \code{marray}--, but the correct subsetting
#' behaviour is only obtained when applied to \code{marray}s.
#' 
#' In all other respects, \code{marray}s behave exactly as standard
#' \code{array}s.
#' }
#' 
#' @title Arrays with Metadata along Dimensions
#' @name marray
#' @aliases is.marray as.marray as.marray.default as.marray.marray
#' 	print.marray
#' @usage
#' marray(data = NA, dim = length(data), dimnames = NULL,
#' 		dimdata = NULL)
#' 
#' as.marray(x, ...)
#' \method{as.marray}{default}(x, dimdata = NULL, ...)
#'  
#' is.marray(x, ...)
#' 
#' @param data a vector (including a list) giving data to fill the array.
#' @param dim the dim attribute for the array to be created, that is a vector 
#' 	of length one or more giving the maximal indices in each dimension.
#' @param dimnames either \code{NULL} or the names for the dimensions. 
#' 	This is a list with one component for each dimension, either \code{NULL} 
#'  or a character vector of the length given by \code{dim} for that dimension. 
#'  The list can be named, and the list names will be used as names for the 
#'  dimensions. If the list is shorter than the number of dimensions, it is 
#'  extended by \code{NULL}s to the length required
#' @param dimdata either \code{NULL} or a list with one component for each
#'  dimension that can be \code{NULL} or an object (such as a vector, list, 
#'	or data frame) whose length equals \code{dim} for that dimension.
#'	If the list is shorter than the number of dimensions, it is 
#'  extended by \code{NULL}s to the length required.
#' @param x an \R object.
#'  
#' @return \code{marray} creates \code{marray}s exactly in the same way as 
#' 	\code{\link[base]{array}} (with the extents specified in \code{dim} and 
#'  naming information in \code{dimnames}), plus additional metadata
#'  for each dimension specified in \code{dimdata}.
#' 
#'  \code{as.marray} coerces objects to class \code{marray}. 
#'  It is a generic function: \link[base:UseMethod]{methods} can be written 
#'  to handle specific classes of objects.
#' 
#'  \code{is.marray} returns \code{TRUE} or \code{FALSE} depending on whether 
#'  its argument is an array and, furthermore, is of class \code{marray}. 
#'  Realize that the \code{dimdata} attribute is optional, so we may have
#'  \code{marray}s without it --although this is not common.
#'  \code{is.marray} is a generic function: \link[base:UseMethod]{methods} 
#'  can be written to handle specific classes of objects.
#' 
#' @author Enrique Bengoechea, based on a suggestion by Henrik Bengtsson 
#'  on the R-devel mailing list.
#'  
#' @seealso \code{\link[base]{array}}.
#' 
#'  "Annotations" on the \pkg{Biobase} package have a similar meaning to
#'  "metadata" as used here. See class 
#'  \code{\link[Biobase]{AnnotatedDataFrame}}.
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

#' @nord
#' @S3method as.marray marray

as.marray.marray <- function(x, ...) 
	x;

#' @nord
#' @S3method as.array marray

as.array.marray <- function(x, ...) { 
	attr(x, "dimdata") <- NULL;
	class(x) <- NULL;
	x;
}

#' @nord
#' @S3method print marray

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

