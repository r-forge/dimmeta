################################################################################
# R PACKAGE:   metadata
# FILE:        R/marray_subset.R
# DESCRIPTION: Methods related to extraction and replacement of 
#			   parts of 'marray' objects.  
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  2009 July 7
# LICENSE:     GPL-2
################################################################################

#' Extract or replace subsets of objects with metadata.
#' 
#' These methods behave as their \code{data.frame}  
#' \link[base:Extract.data.frame]{counterparts}, with the following 
#' differences:
#' \itemize{
#' 	\item for extraction, whenever possible, the \code{marray} class 
#'  	(or subclass) and its associated metadata are preserved, subsetting
#' 		also the metadata as necessary. If some component of the key is 
#' 		dropped, the whole key is removed from the metadata.
#' 		Subsets extracted with \code{[[} and \code{$}, or with \code{[} if 
#' 		some dimension is dropped, always lose the metadata. 	 
#' 	\item for replacement, the metadata constraints are enforced: 
#' 	\enumerate{
#' 		\item data is coerced to the target column class automatically
#' 		\item \code{NA}s in columns with \code{na.ok = FALSE} raise an
#' 			error. Beware that non-intended \code{NA}s may appear in the
#' 			automatic coercion process.   			
#' 		\item new data is filled with the default value when a specific
#' 			value is not given.
#' 		\item duplicated key values in several rows raise an error.
#' 	}
#' }
#' 
#' @title Extract or Replace Parts of a Specialized Data Frame
#' @name Extract.marray
#' @aliases [.marray [<-.marray [[.marray [[<-.marray
#' @usage 
#' 	\method{[}{marray}(x, ..., drop)
#'  \method{[}{marray}(x, ...) <- value
#' 	\method{[[}{marray}(x, ...) <- value
#'  
#' @param x an \code{\link{marray}}.
#' @param {i,j} elements to extract or replace. For \code{[} and \code{[[}, 
#' 	these are \code{numeric} or \code{character} or, for \code{[} only, 
#'  \code{logical} or \code{empty}. Numeric values are coerced to 
#'  \code{integer}. 
#  For replacement by [, a logical matrix is allowed. For replacement by $, 
#  i is a name or literal character string.
#' @param drop logical. If \code{TRUE} the result is coerced to the lowest 
#'  possible dimension. The default is to drop if only one column is left, 
#'  but not to drop if only one row is left. If some dimension is dropped,
#'  class \code{marray} and the related metadata are always dropped.
#' @param value a suitable replacement value: it will be repeated a whole 
#'  number of times if necessary and it may be coerced to the column
#'  class in the metadata specification. If \code{NULL}, deletes the column 
#'  if a single column is selected. 
#' 
#' @return For \code{[} a marray, data frame, list or a single column 
#'  (the latter two only when dimensions have been dropped). 
#' @seealso \link[base:Extract.data.frame]{Extract} for data frames.
#' @S3method `[` marray

#' @S3method `[` marray
#  NOTE: The replacement method is not needed: indexes outside bounds 
# 		raise an error with arrays

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