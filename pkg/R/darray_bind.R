################################################################################
# R PACKAGE:   dimmeta
# FILE:        R/darray_bind.R
# DESCRIPTION: Combine darrays by rows or columns. 
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  22/07/2009
# LICENSE:     GPL-2
################################################################################

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
	if (!length(dots) || all(sapply(dots, is.null, USE.NAMES=FALSE)))
		return(NULL);
	
	# Bind the metadata if required
	bindDims <- bindLengths(dots, bind.dim=1L);
	if (is.null(bindDims))
		stop("number of columns of matrices must match");
	if (has2dims <- any(bindDims$has2dims)) {
		rowMeta <- lapply(dots, function(y) dimmeta(y)[[1L]]);
		modelIndex <- metabindModel(rowMeta);
		tryCatch(rowMeta <- 
				metabind(rowMeta[[modelIndex]], rowMeta, bindDims$bind),
			error = function(e) 
				stop("cannot combine row metadata, ", e, call.=FALSE));

		colMeta <- dimmeta(dots[[modelIndex]])[[2L]];
	}
	
	# Drop darray class to avoid the built-in rbind special dispatching
	isMarray <- sapply(dots, is.darray, USE.NAMES=FALSE);
	dots[isMarray] <- lapply(dots[isMarray], as.array); 		
	
	x <- do.call("rbind", c(dots, list(deparse.level=deparse.level)));
	
	if (has2dims) 
		x <- as.darray(x, dimmeta=list(rowMeta, colMeta));			
	x;
} 

#' @nord
#' @S3method cbind darray

cbind.darray <- function(..., deparse.level=1) {
	dots <- list(...);
	if (!length(dots) || all(sapply(dots, is.null, USE.NAMES=FALSE)))
		return(NULL);
	
	# Bind the metadata if required
	bindDims <- bindLengths(dots, bind.dim=2L);
	if (is.null(bindDims))
		stop("number of rows of matrices must match");
	if (has2dims <- any(bindDims$has2dims)) {
		colMeta <- lapply(dots, function(y) dimmeta(y)[[2L]]);
		modelIndex <- metabindModel(colMeta);
		tryCatch(colMeta <- 
				metabind(colMeta[[modelIndex]], colMeta, bindDims$bind),
			error = function(e) 
				stop("cannot combine row metadata, ", e, call.=FALSE));

		rowMeta <- dimmeta(dots[[modelIndex]])[[1L]];
	}
	
	# Drop darray class to avoid the built-in cbind special dispatching
	isMarray <- sapply(dots, is.darray, USE.NAMES=FALSE);
	dots[isMarray] <- lapply(dots[isMarray], as.array); 		
	
	x <- do.call("cbind", c(dots, list(deparse.level=deparse.level)));
	
	if (has2dims) 
		x <- as.darray(x, dimmeta=list(rowMeta, colMeta));			
	x;
}

#' This internal helper function computes the number of items
#' supplied by each element being bound according to the standard rules
#' of \code{\link{cbind}} and \code{rbind}.  
#'  
#' @title Number of Items Supplied by Each Object Being Bound
#' @param x list of objects to be combined (by \code{rbind} or \code{cbind})
#' @param dim integer with the dimension on which to do the extraction:
#' 	1 for \code{rbind} and 2 for \code{cbind}.
#' @return A list with three elements:
#'  \item{bind}{an integer vector with the same length as \code{x}, with
#' 		the number of items added to the result of the bind operation by
#' 		each element of \code{x}: rows for \code{rbind} and columns for 
#' 		\code{cbind}.}
#'  \item{keep}{an integer scalar with the length of the non-binded 
#' 		dimension that would result from the bind operation: rows for 
#' 		\code{cbind} and columns for \code{rbind}.}
#'  \item{has2dims}{logical. Whether any of the input items has two
#' 		dimensions.}  
#' 
#' @note This is a helper non-exported function to be used internally by 
#' 	\code{rbind.darray} and \code{cbind.darray}. 
#' @nord
#  NOT EXPORTED

bindLengths <- function(x, bind.dim=1L) {
	keepDim <- if (bind.dim == 1L) 2L else 1L;
	
	dims <- lapply(x, dim);	
	xLens <- sapply(x, length, USE.NAMES=FALSE);
	dimsBind <- ifelse(xLens, 1L, 0L);

	has2dims <- sapply(dims, function(y) length(y) >= 2L, USE.NAMES=FALSE);	
	if (any(has2dims)) {
		keepDimLen <- unique(sapply(dims[has2dims], .subset, keepDim));
    	if (length(keepDimLen) != 1L)
			return(NULL);
		
		dimsBind[has2dims] <- sapply(dims[has2dims], .subset, bind.dim, 
			USE.NAMES=FALSE);
		list(bind=dimsBind, keep=keepDimLen[[1L]], has2dims=has2dims)
	} else  		
		list(bind=dimsBind, keep=max(xLens, na.rm=TRUE), has2dims=has2dims);
}

#' Determines the "most complex" object within a list of dimension-sensitive
#' metadata objects, to be used as the model for both the metabind dispatching 
#' and the length/name for each dimension.
#' 
#' This function sorts objects in the supplied list where each of the
#' following is considered more complex than the previous one.
#' \begin{enumerate}{
#' 	\item \code{NULL}s
#' 	\item plain atomic vectors
#'  \item S3 class extending an atomic vector
#' 	\item plain list (recursive) vectors
#'  \item S3 class extending a list vector
#'  \item vectors with dimensions (a "\code{dim}" attribute), e.g. arrays
#'  \item S3 class extending vectors with dimensions, e.g. data frames 
#' }
#' 
#' In case several objects are evaluated to have a similar "degree of
#' complexity" (e.g. if there are several data frames), the first one
#' appearing in the list is returned.
#' 
#' @title Determine the "Most Complex" Metadata Object 
#' @param x a list of metadata objects.
#' @return The index (position) of the element of \code{x} that is 
#'  considered "most complex" and should be used as the model for the binding.
#' 
#' @note This is a helper non-exported function to be used internally by 
#' 	\code{rbind.darray} and \code{cbind.darray}. 
#   NOT EXPORTED

metabindModel <- function(x) {
	metaComplexityIndex <- sapply(x, function(m) {
			if (is.null(m)) 
				-1L
			else 
				as.integer(!is.null(oldClass(m))) + 
					if (length(dim(m))) 3L + length(dim(m))
					else if (is.atomic(m)) 0L
					else 2L
		}, USE.NAMES=FALSE);
	
	which.max(metaComplexityIndex)[1L];
} 

#' @nord
#' @S3method metabind default

metabind.default <- function(model, meta, lengths) {
	isNull <- sapply(meta, is.null); 
	if (all(isNull))
		return(NULL)
	else if (any(isNull))
		meta[isNull] <- mapply(function(y, len) rep(NA, len),
			y=meta[isNull], len=lengths[isNull], 
			SIMPLIFY=FALSE, USE.NAMES=FALSE);
    
	unlist(meta);
}

#' @nord
#' @S3method metabind list

metabind.list <- function(model, meta, lengths) {
	if (any(isNull <- sapply(meta, is.null))) 
		meta[isNull] <- mapply(function(y, len) vector(mode="list", len),
			y=meta[isNull], len=lengths[isNull], 
			SIMPLIFY=FALSE, USE.NAMES=FALSE);
	
	do.call("c", meta);
}

#' @nord
#' @S3method metabind matrix

metabind.matrix <- function(model, meta, lengths) {
	if (any(isNull <- sapply(meta, is.null)))
		meta[isNull] <- mapply(function(y, len) {
				# Create metadata structure with the adequate 
				# dimensions and filled with NAs				
				result <- matrix(NA, len, NCOL(model),
					dimnames=list(NULL, colnames(model)))					
			}, y=meta[isNull], len=lengths[isNull], 
			SIMPLIFY=FALSE, USE.NAMES=FALSE);

	do.call("rbind", meta);
}

#' @nord
#' @S3method metabind data.frame

metabind.data.frame <- function(model, meta, lengths) {
	if (any(isNull <- sapply(meta, is.null)))
		meta[isNull] <- mapply(function(y, len) {
				result <- model[seq_len(len),,drop=FALSE];
				if (len > 0L) {					
					rownames(result) <- NULL;
					# Create metadata structure with the adequate 
					# dimensions and filled with NAs, preserving					
					# each variable class (e.g. factors --direct NA  
					# assignment would drop them) 
					result <- lapply(result, `[<-`, value=NA);
				}
			}, y=meta[isNull], len=lengths[isNull], 
			SIMPLIFY=FALSE, USE.NAMES=FALSE);

	result <- do.call("rbind", meta);
	
	if (!is.null(rownames(result)))
		rownames(result) <- NULL;
	result;	
}
