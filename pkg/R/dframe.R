################################################################################
# R PACKAGE:   dimmeta
# FILE:        R/dframe.R
# DESCRIPTION: S3 class 'dframe'. 
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  24/07/2009
# LICENSE:     GPL-2
################################################################################

#' Creates or tests for data frames with metadata along their dimensions.
#' 
#' \code{dframe} is an S3 class that provides infrastructure to add metadata 
#' to each dimension extent of a data frame. \code{dframe}s operate in most 
#' respects like standard data frames, with the addition of attribute 
#' \code{dimmeta} whose behaviour closely matches that of \code{dimnames}. 
#' 
#' Each component of \code{dimmeta} can be any \R object that has methods for 
#' the subscripting \code{[} operator, provided that:
#' \enumerate{
#' 	\item if the object has no dimension, its length equals the length
#' 		of the data frame for the corresponding dimension. For example,
#' 		atomic vectors or lists.
#'	\item if the object has dimensions (i.e. a \code{dim} attribute),
#' 		its number of items of the first dimension equals the length of the
#' 		data frame for the corresponding dimension. For example, another data
#' 		frame, or an array (where the number of rows would match the
#' 		number of elements on the array dimension).
#' } 		 
#' 
#' Metadata along the dimensions is subsetted together with the data frame, 
#' exactly as \code{dimname}s. It is lost when dimensions are dropped on
#' subsetting operations.    
#' 
#' \link[base:UseMethod]{Methods} to 
#' \link[base:InternalMethods]{standard generics} for "\code{dframe}" objects 
#' currently include: 
#' \code{\link[=print.dframe]{print}}, the 
#' \code{\link[=Extract.dframe]{[}} extract operator, 
#' \code{\link[=rbind.dframe]{rbind}} and 
#' \code{cbind}. All these allow to keep the metadata along the dimensions
#' in sync with manipulations to the data frame.
#'
#' Modifying the dimensions after creation with \code{dim<- }is prevented 
#' for obvious reasons: how to change \code{dimmeta} would be undefined
#' in most cases. 
#' 
#' New generic functions \code{\link{dimmeta}} and \code{dimmeta<-} allow 
#' to get and replace the metadata. The default implementations
#' get and set the \code{dimmeta} attribute --irrespective of whether 
#' the object is of class \code{dframe}--, but the correct subsetting
#' behaviour is only obtained when applied to \code{dframe}s.
#' 
#' Wrappers to manipulate the metadata of the two first dimensions are
#' provided with generics \code{\link{rowmeta}} and \code{\link{colmeta}},
#' --in both setting and replacement versions.
#' 
#' In all other respects, \code{dframe}s behave exactly as standard
#' \code{data.frame}s.
#' 
#' @title Data Frames with Metadata along Dimensions
#' @name dframe
#' @aliases dframe is.dframe as.dframe as.dframe.default as.dframe.data.frame
#' 	print.dframe
#' @usage
#' dframe(\dots, row.names = NULL, check.rows = FALSE, check.names = TRUE,
#' 		stringsAsFactors = default.stringsAsFactors(), dimmeta = NULL)
#' 
#' as.dframe(x, ...)
#' \method{as.dframe}{default}(x, dimmeta = NULL, ...)
#'  
#' is.dframe(x, ...)
#' 
#' @param \dots these arguments are of either the form \code{value} or 
#' 	\code{tag = value}. Component names are created based on the tag 
#' 	(if present) or the deparsed argument itself.
#' @param row.names \code{NULL} or a single integer or character string 
#' 	specifying a column to be used as row names, or a character or integer 
#' 	vector giving the row names for the data frame.
#' @param check.rows if \code{TRUE} then the rows are checked for 
#' 	consistency of length and names.
#' @param logical. If \code{TRUE} then the names of the variables in the 
#' 	data frame are checked to ensure that they are syntactically valid 
#' 	variable names and are not duplicated. If necessary they are adjusted 
#'  (by \code{make.names}) so that they are. 
#' @param stringsAsFactors logical: should character vectors be converted to 
#' 	factors? The ‘factory-fresh’ default is \code{TRUE}, but this can be 
#'  changed by setting \code{\link{options}(stringsAsFactors = FALSE)}. 
#' @param dimmeta either \code{NULL} or a list of length 1 or 2 providing
#' 	the rows and columns metadata, respectively. Each list 
#' 	component can be \code{NULL}, an object without dimensions whose 
#' 	length equals the number of rows / columns in the data frame, or an 
#' 	object with dimensions whose number of elements in the first dimension
#'  equals the number of rows / columns.
#' 
#' @param x an \R object.
#'  
#' @return \code{dframe} creates \code{data.frame}s exactly in the same way as 
#' 	\code{\link{data.frame}}, plus additional metadata varying along the
#' 	rows and/or columns of the data frame..
#' 
#'  \code{as.dframe} coerces objects to class \code{dframe}. 
#'  It is a generic function: \link[base:UseMethod]{methods} can be written 
#'  to handle specific classes of objects.
#' 
#'  \code{is.dframe} returns \code{TRUE} or \code{FALSE} depending on whether 
#'  its argument is a data frame and, furthermore, is of class \code{dframe}. 
#'  Realize that the \code{dimmeta} attribute is optional, so we may have
#'  \code{dframe}s without it --although this is not common.
#'  \code{is.dframe} is a generic function: \link[=UseMethod]{methods} 
#'  can be written to handle specific classes of objects.
#' 
#' @author Enrique Bengoechea, based on a suggestion by Henrik Bengtsson 
#'  on the R-devel mailing list. Thanks also to Tony Plate, Heinz Tuechler,
#'  and Laurent Gautier for their feedback.
#' @seealso \code{\link{data.frame}}, \code{\link{darray}}.
#' 
#'  "Annotations" on the \pkg{Biobase} package have a similar meaning to
#'  "metadata" as used here. See class 
#'  \code{\link[Biobase]{AnnotatedDataFrame}}.
#' @export

dframe <- function(..., row.names = NULL, check.rows = FALSE, 
		check.names = TRUE, stringsAsFactors = default.stringsAsFactors(), 
		dimmeta = NULL) {
	data <- data.frame(..., row.names=row.names, check.rows=check.rows, 
		check.names=check.names, stringsAsFactors=stringsAsFactors); 
	if (length(dimmeta))
		dimmeta(data) <- dimmeta;
	class(data) <- c("dframe", "data.frame");
	data;
}

#' @nord
#' @export

is.dframe <- function(x) 
	is.data.frame(x) && inherits(x, "dframe");

#' @nord
#' @S3method as.dframe default

as.dframe.default <- function(x, dimmeta = NULL, ...) {
	if (!is.data.frame(x))
		x <- as.data.frame(x);
	if (length(dimmeta))
		dimmeta(x) <- dimmeta;
	class(x) <- c("dframe", "data.frame");
	x;
}

#' @nord
#' @S3method as.dframe dframe

as.dframe.dframe <- function(x, ...) 
	x;

#' @nord
#' @S3method as.data.frame dframe

as.data.frame.dframe <- function(x, ...) {
	if (!is.null(attr(x, "dimmeta")))
		attr(x, "dimmeta") <- NULL;
	class(x) <- "data.frame";
	x;
}

#' @nord
#' @S3method print dframe

print.dframe <- function(x, ...) {
	# This title is currently used only to facilitate debugging. 
	# Probably not needed on release version...
    cat("[data frame with dimensional attributes]\n");
	y <- as.data.frame(x);
	NextMethod();
	invisible(y);
}
