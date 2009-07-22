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
