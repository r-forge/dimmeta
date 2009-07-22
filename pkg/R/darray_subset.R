################################################################################
# R PACKAGE:   dimmeta
# FILE:        R/darray_subset.R
# DESCRIPTION: Methods related to extraction and replacement of 
#			   parts of 'darray' objects.  
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  2009 July 7
# LICENSE:     GPL-2
################################################################################

#' Extract subsets of arrays with metadata.
#' 
#' This method behaves as its \code{array} \link[base:Extract]{counterpart}, 
#' with the additional feature that the \code{dimmeta} attribute, when
#' present, is subsetted along with each of the corresponding dimension
#' subscripts.
#' 
#' @title Extract Parts of Arrays with Metadata
#' @name Extract.darray
#' @aliases Extract.darray [.darray 
#' @usage 
#' 	\method{[}{darray}(x, ..., drop = TRUE)
#'  
#' @param x an \code{\link{darray}}.
#' @param \dots indexes for elements to extract: these can
#' 	be either non-specified, or \code{numeric}, \code{logical}, or 
#'  \code{character} vectors, as for standard arrays. 
#' @param drop logical. If \code{TRUE} the result is coerced to the lowest 
#'  possible dimension, as for standard arrays.   
#'  If the dimensions are actually dropped, attribute \code{dimmeta} is also
#' 	dropped, losing all the metadata. 
#' @param value a suitable replacement value: it will be repeated a whole 
#'  number of times if necessary.	 
#' 
#' @seealso \link[base:Extract]{[} operator for arrays.
#' @S3method `[` darray
#  NOTE: The replacement method is not needed: indexes outside bounds 
# 		 raise an error with arrays.

`[.darray` <- function(x, ..., drop = TRUE) {
	dimMeta <- dimmeta(x);
	dimNames <- dimnames(x);
	origXDim <- dim(x); 
	
	x <- NextMethod();
	nDimX <- length(dim(x)); 
	
	# If there's dimmeta and the result has not been dropped to
    # a vector, we need to also subset the dimmeta.
	if (length(dimMeta) && nDimX) {
		# Detect missing index arguments and replace by TRUE 		
		indexes <- lapply(substitute(alist(...))[-1L],  
				function(y) 
					if (is.name(y) && identical("", as.character(y)))
	    				TRUE
					else eval(y)
			);
			
		# If some dimension has been dropped, remove its metadata
		if (length(origXDim) > nDimX) 			
			for (i in seq.int(length(origXDim), 1, by=-1))
				if (indexLenSelection(indexes[[i]], origXDim[[i]]) == 1L) {
					dimMeta[[i]] <- NULL;
					indexes[[i]] <- NULL;
					dimNames <- dimNames[-i];
				}
		
		# Subset the dimmeta		
		doSubset <- mapply(function(meta, index) 
				length(meta) && !identical(TRUE, index),
			meta=dimMeta, index=indexes, USE.NAMES=FALSE);
		for (i in which(doSubset)) {
    		index <- indexes[[i]];
			if (is.character(index))
				index <- match(index, dimNames[[i]], nomatch=0L)
			meta <- dimMeta[[i]];
    		dimMeta[[i]] <- if (length(dim(meta))) 
				meta[index,,drop=FALSE]
			else meta[index]
		}

		attr(x, "dimmeta") <- dimMeta;
		class(x) <- "darray";
	}	
			
	x;		
}

#' Methods for combining \code{darray}s with other objects by rows or columns. 
#' 
#' These methods behave as \code{rbind} and \code{cbind}, with the
#' additional feature that attribute \code{dimmeta} (with metadata along
#' the dimensions) is kept whenever possible. When the combined objects
#' are not \code{darray}s, each dimension metadata is filled with either
#' \code{NA}s (for atomic vectors) or \code{NULL}s (for lists). 
#' 
#' @title Combine darrays by Rows or Columns
#' @aliases rbind.darray cbind.darray
#' @usage
#'  \method{rbind}{darray}(..., deparse.level = 1)
#' 
#'  \method{cbind}{darray}(..., deparse.level = 1)
#' 
#' @param \dots objects to be combined.
#' @param deparse.level integer controlling the construction of labels in 
#' 	the case of non-matrix-like arguments: see \code{\link[=cbind]{rbind}}. 
#' 
#' @return An \code{darray} with 2 dimensions (an "mmatrix"!) combining the 
#' 	\dots arguments column-wise or row-wise.
#'  
#'  For \code{cbind} row data is taken from the first argument with 
#'  appropriate "rowmeta" on its \code{dimmeta} attribute. Column data
#'  is built combining each argument's "colmeta", filled with \code{NA}s or
#'  \code{NULL}s for arguments that lack a \code{dimmeta} attribute.
#' 
#'  For \code{rbind} column data is taken from the first argument with 
#'  appropriate "colmeta" on its \code{dimmeta} attribute. Row data
#'  is built combining each argument's "rowmeta", filled with \code{NA}s or
#'  \code{NULL}s for arguments that lack a \code{dimmeta} attribute.
#' 
#' @S3method rbind darray

rbind.darray <- function(..., deparse.level=1) {
	dots <- list(...);	
	if (length(dots))
		dots <- dots[sapply(dots, length) > 0L];
	
	isList <- FALSE;
	allNull <- TRUE;	
    if (length(dots)) {
		rowMeta <- lapply(dots, function(x) {
				if (is.matrix(x)) {
					if (is.darray(x)) {
						result <- dimmeta(x)[[1L]];
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
		
		colMeta <- NULL;
		i <- 1; 
		while (is.null(colMeta) && i <= length(dots)) {
			x <- dots[[i]];
			if (is.matrix(x) && is.darray(x) && !is.null(dimmeta(x)[[2L]]))
				colMeta <- dimmeta(x)[[2L]];
    		i <- i + 1;
		} 
	}

	x <- do.call("rbind", 
		c(lapply(dots, as.array), list(deparse.level=deparse.level)));
	
	if (length(dots) && is.matrix(x) && !allNull) {
		rowMeta <- if (isList) 
			do.call("c", rowMeta)				
		else 
			unlist(lapply(rowMeta, function(x) {
					if (is.null(x)) 
						NA
					else if (is.list(x) && all(sapply(x, is.null)))
						rep(NA, length(x))
					else x
				}));
		x <- as.darray(x, dimmeta=list(rowMeta, colMeta));		
	}
	
	x;
} 

#' @nord
#' @S3method cbind darray

cbind.darray <- function(..., deparse.level=1) {
	dots <- list(...);	
	if (length(dots))
		dots <- dots[sapply(dots, length) > 0L];
	
	isList <- FALSE;
	allNull <- TRUE;	
    if (length(dots)) {
		colMeta <- lapply(dots, function(x) {
				if (is.matrix(x)) {
					if (is.darray(x)) {
						result <- dimmeta(x)[[2L]];
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
		
		rowMeta <- NULL;
		i <- 1; 
		while (is.null(rowMeta) && i <= length(dots)) {
			x <- dots[[i]];
			if (is.matrix(x) && is.darray(x) && !is.null(dimmeta(x)[[1L]])) {
				rowMeta <- dimmeta(x)[[1L]];
				allNull <- FALSE;
			}
    		i <- i + 1;
		} 
	}
	
	x <- do.call("cbind", 
		c(lapply(dots, as.array), list(deparse.level=deparse.level)));

	if (length(dots) && is.matrix(x) && !allNull) {
		colMeta <- if (isList) 
			do.call("c", colMeta)				
		else 
			unlist(lapply(colMeta, function(x) {
					if (is.null(x)) 
						NA
					else if (is.list(x) && all(sapply(x, is.null)))
						rep(NA, length(x))
					else x
				}));
		x <- as.darray(x, dimmeta=list(rowMeta, colMeta));		
	}
		
	x;
}

#' Returns the length of the items selected by index \code{x} when used
#' to subscript an object whole length or dimension length being subscripted
#' is given by \code{len}.
#'
#' @title Number of Elements Selected by an Index
#' @param x a numeric, logical, or character vector used as subscript 
#' 	to index an object.
#' @param len integer with the length of the object to be indexed, or the
#' 	length of the dimension that is to be indexed.
#' 
#' @return An integer with the number of elements that would result
#' 	from the selection.  
#' @note This function is private, only to be invoked internally by 
#' \code{[.darray}: it relies on the fact that the index has been previously
#' validated by the primitive indexing, otherwise we would need much more 
#' checks!
#' 
#' @nord  
#  NOT EXPORTED 

indexLenSelection <- function(x, len) {
	if (is.character(x))
		length(x)
	else if (is.logical(x))
		sum(rep(x, length.out=len))
	else if (is.numeric(x))
		if (all(x >= 0))
			sum(as.integer(x) != 0L)
		else
			length(seq_len(len)[x])
}

#' @name dimReplace.darray
#' @nord
#' @S3method `dim<-` darray

`dim<-.darray` <- function(x, value) 
    stop("modifying dimensions is not supported for darrays: coerce to array first");