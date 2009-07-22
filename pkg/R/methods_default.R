################################################################################
# R PACKAGE:   metadata
# FILE:        R/default_methods.R
# DESCRIPTION: Default methods for some generics. 
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  08/07/2009
# LICENSE:     GPL-2
################################################################################

#' @title Get or Set Metadata along Array Dimensions
#' @name dimmeta
#' @aliases dimmeta.default `dimmeta<-.default` 
#' 	rowmeta rowmeta.default `rowmeta<-` `rowmeta<-.default`
#' 	colmeta colmeta.default `colmeta<-` `colmeta<-.default`
#' @usage 
#'  dimmeta(x, ...)
#'  \method{dimmeta}{default}(x, use.dimnames = FALSE)
#'
#'  dimmeta(x, ...) <- value 
#'  \method{dimmeta}{default}(x, use.dimnames = FALSE) <- value
#' 
#'  rowmeta(x, ...)
#'  \method{rowmeta}{default}(x, use.rownames = FALSE)
#'
#'  rowmeta(x, ...) <- value 
#'  \method{rowmeta}{default}(x, use.rownames = FALSE) <- value
#' 
#'  colmeta(x, ...)
#'  \method{colmeta}{default}(x, use.colnames = FALSE)
#'
#'  colmeta(x, ...) <- value 
#'  \method{colmeta}{default}(x, use.colnames = FALSE) <- value
#' 
#' @param x an \R object, normally an \code{\link{marray}}.
#' @param use.dimnames logical flag; whether the returned data should
#' 	have the same \code{dimnames} as \code{x}. Metadata stored as objects
#'  with dimensions get the names of their \emph{first dimension} replaced,
#'  while metadata without dimensions get their names replaced.
#' @param use.rownames logical flag; whether the returned data should
#' 	have the same \code{rownames} as \code{x}.
#' @param use.colnames logical flag; whether the returned data should
#' 	have the same \code{colnames} as \code{x}.
#' @param value a suitable replament value. For \code{dimmeta}, 
#'  either \code{NULL} or a list with one component for each
#'  dimension that can be \code{NULL} or an object (such as a vector, list, 
#'	or data frame) whose length equals \code{dim} for that dimension.
#'	If the list is shorter than the number of dimensions, it is 
#'  extended by \code{NULL}s to the length required.
#' 
#'  For \code{rowmeta} and \code{colmeta}, the first and second component
#'  of the \code{dimmeta} list, respectively.
#' 
#' @S3method dimmeta default

dimmeta.default <- function(x, use.dimnames=FALSE) {
	result <- attr(x, "dimmeta")   
	if (use.dimnames) { 
		xDimnames <- dimnames(x);
		result <- if (is.null(xDimnames))
			lapply(result, function(x)
    				if (length(dim(x)))
						dimnames(x) <- NULL
					else names(x) <- NULL)
		else {
			isNotNull <- !sapply(result, is.null, USE.NAMES=FALSE)
			for (i in which(isNotNull))
				if (length(dim(result[[i]])))
					dimnames(result[[i]])[[1L]] <- xDimnames[[i]]
				else names(result[[i]]) <- xDimnames[[i]];
			result;
		}
		names(result) <- names(xDimnames);
		result;
	} else result
}

#' @name dimmetaReplace.default
#' @nord
#' @S3method `dimmeta<-` default

`dimmeta<-.default` <- function(x, value) {
	xDim <- dim(x);
	if (is.null(xDim))
		stop("'%s' cannot be applied to objects without dimensions", "dimmeta");
	
	if (!is.null(value)) {
		if (!is.list(value))
			stop(gettextf("'%s' must be a list", "dimmeta"));
    	
		if (length(value) > length(xDim))
			stop(gettextf("length of '%s' [%d] must match that of '%s' [%d]",
				"dimmeta", length(value), "dims", length(xDim)));
		
		for (i in which(!sapply(value, is.null))) {
			v <- value[[i]];
			if (length(dim(v))) {
    			if (dim(v)[[1L]] != xDim[i])
					stop(gettextf(
						"dim('%s')[[1]] [%d] not equal to array extent",
						"dimmeta", i))						
			} else if (length(v) != xDim[i])
				stop(gettextf("length of '%s' [%d] not equal to array extent",
					"dimmeta", i));	
		}
	
		length(value) <- length(xDim);
	}
	attr(x, "dimmeta") <- value;
	x;
}	

#' @nord
#' @S3method rowmeta default

rowmeta.default <- function(x, use.rownames=FALSE) 
	dimmeta(x, use.dimnames=use.rownames)[[1L]];

#' @name rowmetaReplace.default
#' @nord
#' @S3method `rowmeta<-` default

`rowmeta<-.default` <- function(x, value) {
    dimmeta(x)[[1L]] <- value;
	x;
}

#' @nord
#' @S3method colmeta default

colmeta.default <- function(x, use.colnames=FALSE) 
	dimmeta(x, use.dimnames=use.colnames)[[2L]];

#' @name colmetaReplace.default
#' @nord
#' @S3method `colmeta<-` default

`colmeta<-.default` <- function(x, value) {
    dimmeta(x)[[2L]] <- value;
	x;
}
