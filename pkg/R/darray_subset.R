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
#' @param keep.attrs logical flag or character vector specifying user-defined 
#' 	attributes to preserve. This argument allows users to preserve attributes
#'  which are removed by the default subsetting methods. If \code{TRUE},
#'  all attributes are preserved, or none if \code{FALSE}. If it is a 
#' 	character vector, only the supplied attributes are kept. If a name is
#'  provided that does not correspond to an actual attribute, a warning is
#'  issued.
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

`[.darray` <- function(x, ..., keep.attrs = FALSE, drop = TRUE) {
	dimMeta <- dimmeta(x);
	dimNames <- dimnames(x);
	origXDim <- dim(x);    
	keptAttrs <- keptAttrs(x, keep.attrs, 
		c("class", "dim", "dimnames", "dimmeta"))

	# We need to force copying of x in as.array(x) as NextMethod does not
	# work with the extra argument "keep.attrs"
	x <- .Primitive("[")(as.array(x), ..., drop = drop);
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
		for (i in which(doSubset)) 
			dimMeta[[i]] <- 
				metasubset(dimMeta[[i]], indexes[[i]], dimNames[[i]]);

		attr(x, "dimmeta") <- dimMeta;
		class(x) <- "darray";
		if (length(keptAttrs))
			attributes(x)[names(keptAttrs)] <- keptAttrs;
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

#' This function is used by subsetting methods in the \pkg{dimmeta} package 
#' to determine the set of user-defined attributes to keep.
#' 
#' @title Attributes to Keep on Subset
#' @param x an \R object. 
#' @param keep.attrs specification of attributes to preserve. Either a logical 
#' 	flag or a character vector specifying attributes names. 
#'  If \code{TRUE}, all attributes (except those in \code{standard.attrs}) are 
#' 	preserved, or none if \code{FALSE}. If it is a character vector, only the 
#' 	supplied attributes are kept. If a name is provided that does not 
#' 	correspond to an actual attribute, a warning is issued. 
#' @param standard.attrs character vector of attributes names that should
#' 	be ignored (e.g. "\code{class}", "\code{names}", "\code{dim}", etc.)
#' 
#' @return A list of user-defined attributes of \code{x} to be preserved,
#' 	or \code{NULL} if there are no such attributes.
#' @note This is an internal, non-exported function. 
#' @seealso
#  NOT EXPORTED

keptAttrs <- function(x, keep.attrs = TRUE, 
		standard.attrs=c("class", "names", "row.names", "dim", "dimnames", 
		"dimmeta")) { 
	result <- NULL;
	if (!missing(keep.attrs) && !identical(keep.attrs, FALSE)) {
		attrsNames <- names(attributes(x));
		if (is.logical(keep.attrs)) {
			if (keep.attrs) {
				keep.attrs <- attrsNames[!attrsNames %in% standard.attrs];
				if (length(keep.attrs))
					result <- attributes(x)[keep.attrs];
			}
		} else if (is.character(keep.attrs)) {
			if (any(badAttrName <- !keep.attrs %in% attrsNames)) {
				warning(ngettext(sum(badAttrName),
					"attribute %s not found in 'x'",
					"attributes %s not found in 'x'"),
					paste(keep.attrs[badAttrName], collapse=", "));
				keep.attrs <- keep.attrs[!badAttrName] 
			}				
			if (length(keep.attrs))
				result <- attributes(x)[keep.attrs];
		} else
			stop("'keep.attrs' must be a logical flag or a character vector of attribute names");
	}
	result;
}

#' @name dimReplace.darray
#' @nord
#' @S3method `dim<-` darray

`dim<-.darray` <- function(x, value) 
    stop("modifying dimensions is not supported for darrays: coerce to array first");
