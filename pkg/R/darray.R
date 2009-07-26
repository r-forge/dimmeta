################################################################################
# R PACKAGE:   dimmeta
# FILE:        R/darray.R
# DESCRIPTION: S3 class 'darray' 
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  2009 July 7
# LICENSE:     GPL-2
################################################################################

#' Creates or tests for arrays with metadata along their dimensions.
#' 
#' \code{darray} is an S3 class that provides infrastructure to add metadata 
#' to each dimension extent of an array. \code{darray}s operate in most respects 
#' like standard arrays, with the addition of attribute \code{dimmeta} whose 
#' behaviour closely matches that of \code{dimnames}. 
#' 
#' Each component of \code{dimmeta} can be any \R object that has methods for 
#' the subscripting \code{[} operator, provided that:
#' \enumerate{
#' 	\item if the object has no dimension, its length equals the length
#' 		of the array for the corresponding dimension. For example,
#' 		atomic vectors or lists.
#'	\item if the object has dimensions (i.e. a \code{dim} attribute),
#' 		its number of items of the first dimension equals the length of the
#' 		array for the corresponding dimension. For example, another
#' 		array, or a data frame (where the number of rows would match the
#' 		number of elements on the array dimension).
#' } 		 
#' 
#' Metadata along the dimensions is subsetted together with the array, 
#' exactly as \code{dimname}s. It is lost when dimensions are dropped on
#' subsetting operations.    
#' 
#' \link[base:UseMethod]{Methods} to 
#' \link[base:InternalMethods]{standard generics} for "\code{darray}" objects 
#' currently include: 
#' \code{\link[=print.darray]{print}}, the 
#' \code{\link[=Extract.darray]{[}} extract operator, 
#' \code{\link[=rbind.darray]{rbind}} and 
#' \code{cbind}. All these allow to keep the metadata along the dimensions
#' in sync with manipulations to the array data.
#'
#' Modifying the dimensions after creation with \code{dim<- }is prevented 
#' for obvious reasons: how to change \code{dimmeta} would be undefined
#' in most cases. 
#' 
#' New generic functions \code{\link{dimmeta}} and \code{dimmeta<-} allow 
#' to get and replace the metadata. The default implementations
#' get and set the \code{dimmeta} attribute --irrespective of whether 
#' the object is of class \code{darray}--, but the correct subsetting
#' behaviour is only obtained when applied to \code{darray}s.
#' 
#' Wrappers to manipulate the metadata of the two first dimensions are
#' provided with generics \code{\link{rowmeta}} and \code{\link{colmeta}},
#' --in both setting and replacement versions.
#' 
#' In all other respects, \code{darray}s behave exactly as standard
#' \code{array}s.
#' 
#' @title Arrays with Metadata along Dimensions
#' @name darray
#' @aliases darray is.darray as.darray as.darray.default as.darray.darray
#' 	print.darray
#' @usage
#' darray(data = NA, dim = length(data), dimnames = NULL,
#' 		dimmeta = NULL)
#' 
#' as.darray(x, ...)
#' \method{as.darray}{default}(x, dimmeta = NULL, ...)
#'  
#' is.darray(x, ...)
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
#' @param dimmeta either \code{NULL} or a list whose length equals the number
#' 	of dimensions. Each list component can be \code{NULL}, an object without
#' 	dimensions whose length equals the number of elements in the array
#' 	dimension, or an object with dimensions whose number of elements in
#'  its first dimension equals the number of elements in the array dimension. 
#'	If the list is shorter than the number of dimensions, it is 
#'  extended by \code{NULL}s to the length required.
#' @param x an \R object.
#'  
#' @return \code{darray} creates \code{darray}s exactly in the same way as 
#' 	\code{\link{array}} (with the extents specified in \code{dim} and 
#'  naming information in \code{dimnames}), plus additional metadata
#'  for each dimension specified in \code{dimmeta}.
#' 
#'  \code{as.darray} coerces objects to class \code{darray}. 
#'  It is a generic function: \link[base:UseMethod]{methods} can be written 
#'  to handle specific classes of objects.
#' 
#'  \code{is.darray} returns \code{TRUE} or \code{FALSE} depending on whether 
#'  its argument is an array and, furthermore, is of class \code{darray}. 
#'  Realize that the \code{dimmeta} attribute is optional, so we may have
#'  \code{darray}s without it --although this is not common.
#'  \code{is.darray} is a generic function: \link[=UseMethod]{methods} 
#'  can be written to handle specific classes of objects.
#' 
#' @author Enrique Bengoechea, based on a suggestion by Henrik Bengtsson 
#'  on the R-devel mailing list. Thanks also to Tony Plate, Heinz Tuechler,
#'  and Laurent Gautier for their feedback.
#' @seealso \code{\link{array}}.
#' 
#'  "Annotations" on the \pkg{Biobase} package have a similar meaning to
#'  "metadata" as used here. See class 
#'  \code{\link[Biobase]{AnnotatedDataFrame}}.
#' @examples 
#' x <- darray(1:30, dim = c(2, 3, 5))
#' dimnames(x) <- list(c("a", "b"), c("a1", "a2", "a3"), NULL) 
#' dimmeta(x) <- list(1:2, list(x = 1:5, y = letters[1:8], z = NA), 
#' 		letters[1:5])
#'
#' ## Dimension metadata accessors
#' dimmeta(x)
#' dimmeta(x, use.dimnames = TRUE)
#' rowmeta(x)
#' colmeta(x)
#' 
#' ## Subsetting 
#' y <- x[,1:2,2:3]
#' str(dimnames(y))
#' str(dimmeta(y))
#' 
#' ## Combining by rows or columns 
#' ## (doesn't work with data frames yet)
#' y <- x[,,1]
#' z <- cbind(y, 1, y)
#' str(dimmeta(z))
#' 
#' z <- rbind(y, 1, y)
#' str(dimmeta(z))
#' 
#' ## Using data frames for metadata
#' x <- darray(1:6, dim = c(2, 3))
#' dimmeta(x) <- list(NULL, data.frame(labels=I(c("col1", "col2", "col3")),
#'     units=c("m", "cm", "bar"))) 
#' colmeta(x)$labels
#' colmeta(x)$units
#' 
#' @export

darray <- function (data = NA, dim = length(data), dimnames = NULL,
		dimmeta = NULL) {
	data <- array(data=data, dim=dim, dimnames=dimnames);
	if (length(dimmeta))
		dimmeta(data) <- dimmeta;
	class(data) <- "darray";
	data;
}

#' @nord
#' @export

is.darray <- function(x) 
	is.array(x) && inherits(x, "darray");

#' @nord
#' @S3method as.darray default

as.darray.default <- function(x, dimmeta = NULL, ...) {
	if (!is.array(x))
		x <- as.array(x);
	if (length(dimmeta))
		dimmeta(x) <- dimmeta;
	class(x) <- "darray";
	x;
}

#' @nord
#' @S3method as.darray darray

as.darray.darray <- function(x, ...) 
	x;

#' @nord
#' @S3method as.array darray

as.array.darray <- function(x, ...) { 
	attr(x, "dimmeta") <- NULL;
	class(x) <- NULL;
	x;
}

#' @nord
#' @S3method print darray

print.darray <- function(x, ...) {
	# This title is currently used only to facilitate debugging. 
	# Probably not needed on release version...
    cat("[array with dimensional attributes]\n");
	y <- x;
	x <- unclass(x);
	attr(x, "dimmeta") <- NULL;
	NextMethod();
	invisible(y);
}

