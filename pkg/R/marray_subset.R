################################################################################
# R PACKAGE:   metadata
# FILE:        R/marray_subset.R
# DESCRIPTION: Methods related to extraction and replacement of 
#			   parts of 'marray' objects.  
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  2009 July 7
# LICENSE:     GPL-2
################################################################################

#' Extract subsets of arrays with metadata.
#' 
#' This method behaves as its \code{array} \link[base:Extract]{counterpart}, 
#' with the additional feature that the \code{dimdata} attribute, when
#' present, is subsetted along with each of the corresponding dimension
#' subscript.
#' 
#' @title Extract Parts of Arrays with Metadata
#' @name Extract.marray
# @aliases [.marray 
#' @usage 
#' 	\method{[}{marray}(x, ..., drop = TRUE)
#'  
#' @param x an \code{\link{marray}}.
#' @param \dots indexes for elements to extract: these can
#' 	be either non-specified, or \code{numeric}, \code{logical}, or 
#'  \code{character} vectors, as for standard arrays. 
#  For replacement by [, a logical matrix is allowed. For replacement by $, 
#  i is a name or literal character string.
#' @param drop logical. If \code{TRUE} the result is coerced to the lowest 
#'  possible dimension, as for standard arrays.   
#'  If the dimensions are actually dropped, attribute \code{dimdata} is lost,
#'  losing all the metadata. 
#' @param value a suitable replacement value: it will be repeated a whole 
#'  number of times if necessary.	 
#' 
#' @seealso \link[base:Extract]{[ extraction} for arrays.
#' @S3method `[` marray
#  NOTE: The replacement method is not needed: indexes outside bounds 
# 		 raise an error with arrays.

`[.marray` <- function(x, ..., drop = TRUE) {
	dimData <- dimdata(x);
	y <- x;	# FIXME: This copy is used when indexing by character, due to
			# 	the current arguments of valIndex.character 	
	x <- NextMethod();
	
	if (length(dimData) && length(dim(x))) {
		indexes <- lapply(substitute(alist(...))[-1L],  
				function(y) 
					if (is.name(y) && identical("", as.character(y)))
	    				TRUE
					else eval(y)
			);			
		
		dimAttrLens <- sapply(dimData, length, USE.NAMES=FALSE);
		for (i in seq_along(indexes)) {
    		index <- indexes[[i]];
			if (is.character(index))
				index <- valIndex(index, y, dim=i, retclass="integer")			
			if (dimAttrLens[[i]] && !identical(TRUE, index)) {
				dimData[[i]] <- dimData[[i]][index];
			}
		}

		attr(x, "dimdata") <- dimData;
		class(x) <- "marray";
	}	
			
	x;		
}

#' Methods for combining \code{marray}s with other objects by rows or columns. 
#' 
#' These methods behave as \code{rbind} and \code{cbind}, with the
#' additional feature that attribute \code{dimdata} (with metadata along
#' the dimensions) is kept whenever possible. When the combined objects
#' are not \code{marray}s, each dimension metadata is filled with either
#' \code{NA}s (for atomic vectors) or \code{NULL}s (for lists). 
#' 
#' @title Combine marrays by Rows or Columns
#' @aliases cbind.marray
#' @usage
#'  \method{rbind}{marray}(..., deparse.level = 1)
#'  \method{cbind}{marray}(..., deparse.level = 1)
#' 
#' @param \dots
#' @param deparse.level
#' 
#' @return An \code{marray} with 2 dimensions (an "mmatrix") combining the 
#' 	\dots arguments column-wise or row-wise.
#'  
#'  For \code{cbind} row data is taken from the first argument with 
#'  appropriate "rowdata" on its \code{dimdata} attribute. Column data
#'  is built combining each argument's "coldata", filled with \code{NA}s or
#'  \code{NULL}s for arguments that lack a \code{dimdata} attribute.
#' 
#'  For \code{rbind} column data is taken from the first argument with 
#'  appropriate "coldata" on its \code{dimdata} attribute. Row data
#'  is built combining each argument's "rowdata", filled with \code{NA}s or
#'  \code{NULL}s for arguments that lack a \code{dimdata} attribute.
#' 
#' @S3method rbind marray

rbind.marray <- function(..., deparse.level=1) {
	dots <- list(...);	
	if (length(dots))
		dots <- dots[sapply(dots, length) > 0L];
	
	isList <- FALSE;
	allNull <- TRUE;	
    if (length(dots)) {
		rowData <- lapply(dots, function(x) {
				if (is.matrix(x)) {
					if (is.marray(x)) {
						result <- dimdata(x)[[1L]];
						if (is.list(result))
							isList <<- TRUE;
						if (!is.null(result))
							allNull <<- FALSE; 
						result;
					} else 
						vector(mode="list", nrow(x))
				} else if (is.vector(x)) {
					list(NULL)
				} else NULL;
			});
		
		colData <- NULL;
		i <- 1; 
		while (is.null(colData) && i <= length(dots)) {
			x <- dots[[i]];
			if (is.matrix(x) && is.marray(x) && !is.null(dimdata(x)[[2L]]))
				colData <- dimdata(x)[[2L]];
    		i <- i + 1;
		} 
	}

	x <- do.call("rbind", 
		c(lapply(dots, as.array), list(deparse.level=deparse.level)));
	
	if (length(dots) && is.matrix(x) && !allNull) {
		rowData <- if (isList) 
			do.call("c", rowData)				
		else 
			unlist(lapply(rowData, function(x) {
					if (is.null(x)) 
						NA
					else if (is.list(x) && all(sapply(x, is.null)))
						rep(NA, length(x))
					else x
				}));
		x <- as.marray(x, dimdata=list(rowData, colData));		
	}
	
	x;
} 

#' @nord
#' @S3method cbind marray

cbind.marray <- function(..., deparse.level=1) {
	dots <- list(...);	
	if (length(dots))
		dots <- dots[sapply(dots, length) > 0L];
	
	isList <- FALSE;
	allNull <- TRUE;	
    if (length(dots)) {
		colData <- lapply(dots, function(x) {
				if (is.matrix(x)) {
					if (is.marray(x)) {
						result <- dimdata(x)[[2L]];
						if (is.list(result))
							isList <<- TRUE;
						if (!is.null(result))
							allNull <<- FALSE; 
						result;
					} else 
						vector(mode="list", nrow(x))
				} else if (is.vector(x)) {
					list(NULL)
				} else NULL;
			});
		
		rowData <- NULL;
		i <- 1; 
		while (is.null(rowData) && i <= length(dots)) {
			x <- dots[[i]];
			if (is.matrix(x) && is.marray(x) && !is.null(dimdata(x)[[1L]])) {
				rowData <- dimdata(x)[[1L]];
				allNull <- FALSE;
			}
    		i <- i + 1;
		} 
	}
	
	x <- do.call("cbind", 
		c(lapply(dots, as.array), list(deparse.level=deparse.level)));

	if (length(dots) && is.matrix(x) && !allNull) {
		colData <- if (isList) 
			do.call("c", colData)				
		else 
			unlist(lapply(colData, function(x) {
					if (is.null(x)) 
						NA
					else if (is.list(x) && all(sapply(x, is.null)))
						rep(NA, length(x))
					else x
				}));
		x <- as.marray(x, dimdata=list(rowData, colData));		
	}
		
	x;
}