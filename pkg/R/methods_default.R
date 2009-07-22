################################################################################
# R PACKAGE:   metadata
# FILE:        R/default_methods.R
# DESCRIPTION: Default methods for some generics. 
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  08/07/2009
# LICENSE:     GPL-2
################################################################################

#' @nord
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
