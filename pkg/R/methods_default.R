################################################################################
# R PACKAGE:   dimmeta
# FILE:        R/default_methods.R
# DESCRIPTION: Default methods for some generics. 
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  08/07/2009
# LICENSE:     GPL-2
################################################################################

#' @title Get, Set, or Drop Metadata along Array Dimensions
#' @name dimmeta
#' @aliases dimmeta dimmeta.default `dimmeta<-.default` 
#' 	rowmeta rowmeta.default `rowmeta<-` `rowmeta<-.default`
#' 	colmeta colmeta.default `colmeta<-` `colmeta<-.default`
#'  unmeta
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
#'  unmeta(x, ...)
#'  \method{unmeta}{default}(x)
#' 
#' @param x an \R object, normally an \code{\link{darray}}.
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
			lapply(result, `metanames<-`, value=NULL)
		else {
			isNotNull <- !sapply(result, is.null, USE.NAMES=FALSE)
			for (i in which(isNotNull))
				metanames(result[[i]]) <- xDimnames[[i]];
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
		
		for (i in which(!sapply(value, is.null))) 
			if (metalength(value[[i]]) != xDim[i])
				stop(gettextf(
					"dimmeta length %d (%d) not equal to dimension length (%d)",
					i, metalength(value[[i]]), xDim[i]))						
	
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

#' @nord
#' @S3method unmeta default

unmeta.default <- function(x) {
	if (!is.null(attr(x, "dimmeta")))
		attr(x, "dimmeta") <- NULL;
	x;
}

#' @nord
#' @S3method metalength default

metalength.default <- function(x) {
	if (length(d <- dim(x)))
		d[1L]
	else length(x);	
}

#' @name metalengthReplace.default
#' @nord
#' @S3method `metalength<-` default

`metalength<-.default` <- function(x, value) {
	if (!is.numeric(value) || length(value) != 1L) 
		stop("invalid length value");
	value <- as.integer(value);
	if (value < 0L)
		stop("length cannot be negative");
	
	if (length(d <- dim(x))) {
		nrows <- d[1L];
		if (value > nrows) {
			# abind() adds a "dimnames" attribute even if it is not
			# present in x, so we just ensure dimnames are not modified
			nodmn <- is.null(dimnames(x));
			x <- abind(x, array(NA, c(value-nrows, d[-1L])), along=1L)
			if (nodmn) dimnames(x) <- NULL;
		} else if (value < nrows) {
			iargs <- sapply(d[-1L], function(y) if (y == 0L) 0L else TRUE);			
			x <- do.call("[", c(list(i, seq_len(value)), iargs, drop=FALSE))		
		} 
	} else length(x) <- value;		
	x;
}

#' @name metalengthReplace.default
#' @nord
#' @S3method `metalength<-` default

`metalength<-.data.frame` <- function(x, value) {
	if (!is.numeric(value) || length(value) != 1L) 
		stop("invalid length value");
	value <- as.integer(value);
	if (value < 0L)
		stop("length cannot be negative");

	nrows <- nrow(x);
	if (value > nrows)
		x[value, ] <- NA
	else if (value < nrows)
		x <- x[seq_len(value),, drop=FALSE]
	x;
}

#' @name metanamesReplace.default
#' @nord
#' @S3method `metanames<-` default

`metanames<-.default` <- function(x, value) {
	if (length(dim(x)))
		dimnames(x)[[1L]] <- value
	else names(x) <- value;
	x;
}

#' @nord
#' @S3method metasubset default
#' @note This method must enforce homogeneity of subsetting among different
#' 	R data structures. Specifically, arrays should behave on their dim-data
#'  releted dimension exactly as vectors do, allowing out-of-range indexes
#'  that return NAs (by default, these indexes raise errors with arrays) 

metasubset.default <- function(x, i, nams, exact=TRUE, rm=FALSE) {
	if (is.null(x)) return(NULL);
	metaLen <- metalength(x);
	if (is.character(i)) {
		if (missing(nams)) 
			if (is.null(nams <- names(x)))				
				stop("names are required for character index")
		i <- if (exact) 
			match(i, nams, nomatch=if (rm) 0L else metaLen+1L) 
		else pmatch(i, nams, duplicates.ok=TRUE, 
				nomatch=if (rm) 0L else metaLen+1L);
	}	
	if (rm) 
		i <- invertIndex(i, metaLen);
	
	if (length(d <- dim(x))) {
		# `[` for arrays does not allow out-of-bound indexes, so we bind
    	# one "row" filled with NAs and change indexes to new elements to
		# this new row before invoking the array `[` method.
		isNew <- FALSE;
		if (!rm) {
			if (is.logical(i) && length(i) > metaLen) {
				i <- which(i);
				isNew <- (i > metaLen); 
				i[isNew] <- metaLen + 1;
			} else if (is.numeric(i))
				if (any(isNew <- (as.integer(i) > metaLen)))
					i[isNew] <- metaLen + 1
			if (any(isNew)) {
				# abind() adds a "dimnames" attribute even if it is not
				# present in x, so we just ensure dimnames are not modified
	    		nodmn <- is.null(dimnames(x));
				x <- abind(x, array(NA, c(sum(isNew), d[-1L])), along=1L)
				if (nodmn) dimnames(x) <- NULL;
			}
		}
		iargs <- sapply(d[-1L], function(y) if (y == 0L) 0L else TRUE);			
		do.call("[", c(list(x, i), iargs, drop=FALSE));
	} else x[i];
}

#' @nord
#' @S3method metasubset data.frame

metasubset.data.frame <- function(x, i, nams, exact=TRUE, rm=FALSE) {
	if (is.null(x)) return(NULL);
	metaLen <- metalength(x);
	if (is.character(i)) {		
		if (missing(nams))
			if (is.null(nams <- names(x)))				
				stop("names are required for character index")
		i <- if (exact) 
			match(i, nams, nomatch=if (rm) 0L else metaLen+1L) 
		else pmatch(i, nams, duplicates.ok=TRUE, 
				nomatch=if (rm) 0L else metaLen+1L);
	}
	if (rm) 
		i <- invertIndex(i, metaLen);
	
	x[i,,drop=FALSE];		
}

#' Internal function to inverts a subscripting index, so that it is 
#' interpreted as selecting the elements to remove, rather than those to 
#' subset.
#' 
#' @note This is an internal, non-exported function.
#  NOT EXPORTED

invertIndex <- function(i, len, nams, exact=TRUE) {
	if (is.numeric(i)) {
		i <- as.integer(i)
		if (all(i == 0L))
			seq_len(len)
		else if (all(i >= 0L))
			-i
		else if (all(i <= 0L))
			-seq_len(len)[i]
		else stop("numeric indexes must be all positive or all negative")
	} else if (is.logical(i))
		!i
	else if (is.character(i)) {
		if (missing(nams) || is.null(nams(x)))				
			stop("names are required for character index")		
		if (exact)
			-match(i, nams, nomatch=0L) 
		else -pmatch(i, nams, duplicates.ok=TRUE, nomatch=0L);
	} else
		stop(gettextf("invalid subscript type '%s'", class(i)));
}