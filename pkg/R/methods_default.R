################################################################################
# R PACKAGE:   metadata
# FILE:        R/default_methods.R
# DESCRIPTION: Default methods for some generics. 
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  08/07/2009
# LICENSE:     GPL-2
################################################################################

#' @title Get or Set Metadata along Array Dimensions
#' @name dimdata
#' @aliases dimdata.default `dimdata<-.default` 
#' 	rowdata rowdata.default `rowdata<-` `rowdata<-.default`
#' 	coldata coldata.default `coldata<-` `coldata<-.default`
#' @usage 
#'  dimdata(x, ...)
#'  \method{dimdata}{default}(x, use.dimnames = FALSE)
#'
#'  dimdata(x, ...) <- value 
#'  \method{dimdata}{default}(x, use.dimnames = FALSE) <- value
#' 
#'  rowdata(x, ...)
#'  \method{rowdata}{default}(x, use.rownames = FALSE)
#'
#'  rowdata(x, ...) <- value 
#'  \method{rowdata}{default}(x, use.rownames = FALSE) <- value
#' 
#'  coldata(x, ...)
#'  \method{coldata}{default}(x, use.colnames = FALSE)
#'
#'  coldata(x, ...) <- value 
#'  \method{coldata}{default}(x, use.colnames = FALSE) <- value
#' 
#' @param x an \R object, normally an \code{\link{marray}}.
#' @param use.dimnames logical flag; whether the returned data should
#' 	have the same \code{dimnames} as \code{x}.
#' @param use.rownames logical flag; whether the returned data should
#' 	have the same \code{rownames} as \code{x}.
#' @param use.colnames logical flag; whether the returned data should
#' 	have the same \code{colnames} as \code{x}.
#' @param value a suitable replament value. For \code{dimdata}, 
#'  either \code{NULL} or a list with one component for each
#'  dimension that can be \code{NULL} or an object (such as a vector, list, 
#'	or data frame) whose length equals \code{dim} for that dimension.
#'	If the list is shorter than the number of dimensions, it is 
#'  extended by \code{NULL}s to the length required.
#' 
#'  For \code{rowdata} and \code{coldata}, the first and second component
#'  of the \code{dimdata} list, respectively.
#' 
#' @S3method dimdata default

dimdata.default <- function(x, use.dimnames=FALSE) {
	result <- attr(x, "dimdata")   
	if (use.dimnames) { 
		xDimnames <- dimnames(x);
		result <- if (is.null(xDimnames))
			lapply(result, `names<-`, value=NULL)
		else {
			isNotNull <- !sapply(result, is.null, USE.NAMES=FALSE)
			for (i in which(isNotNull))
				names(result[[i]]) <- xDimnames[[i]];
			result;
		}
		names(result) <- names(xDimnames);
		result;
	} else result
}

#' @name dimdataReplace.default
#' @nord
#' @S3method `dimdata<-` default

`dimdata<-.default` <- function(x, value) {
	xDim <- dim(x);
	if (is.null(xDim))
		stop("'%s' cannot be applied to objects without dimensions", "dimdata");
	
	if (!is.null(value)) {
		if (!is.list(value))
			stop(gettextf("'%s' must be a list", "dimdata"));
    	
		if (length(value) > length(xDim))
			stop(gettextf("length of '%s' [%d] must match that of 's' [%d]",
				"dimdata", length(value), "dims", length(xDim)));
		
		for (i in seq_along(value)) {
			v <- value[[i]];
			if (!is.null(v) && length(v) != xDim[i])
				stop(gettextf("length of '%s' [%d] not equal to array extent",
					"dimdata", i));	
		}
	
		length(value) <- length(xDim);
	}
	attr(x, "dimdata") <- value;
	x;
}	

#' @nord
#' @S3method rowdata default

rowdata.default <- function(x, use.rownames=FALSE) 
	dimdata(x, use.dimnames=use.rownames)[[1L]];

#' @name rowdataReplace.default
#' @nord
#' @S3method `rowdata<-` default

`rowdata<-.default` <- function(x, value) {
    dimdata(x)[[1L]] <- value;
	x;
}

#' @nord
#' @S3method coldata default

coldata.default <- function(x, use.colnames=FALSE) 
	dimdata(x, use.dimnames=use.colnames)[[2L]];

#' @name coldataReplace.default
#' @nord
#' @S3method `coldata<-` default

`coldata<-.default` <- function(x, value) {
    dimdata(x)[[2L]] <- value;
	x;
}
