################################################################################
# R PACKAGE:   dimmeta
# FILE:        R/darray_subset.R
# DESCRIPTION: Methods related to extraction and replacement of 
#			   parts of 'dframe' objects.  
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  2009 July 7
# LICENSE:     GPL-2
################################################################################

#' Extract subsets of data frames with metadata along dimensions.
#' 
#' This method behaves as its \code{data.frame} 
#' \link[base:Extract.data.frame]{counterpart}, 
#' with the additional feature that the \code{dimmeta} attribute, when
#' present, is subsetted along with each of the corresponding dimension
#' subscripts.
#' 
#' @title Extract Parts of Data Frames with Metadata
#' @name Extract.dframe
#' @aliases Extract.dframe [.dframe 
#' @usage 
#' 	\method{[}{dframe}(x, i, j, 
#' 		drop = if (missing(i)) TRUE else length(cols) == 1)
#'  
#' @param x an \code{\link{dframe}}.
#' @param i,j indexes for elements to extract: these can
#' 	be either non-specified, or \code{numeric}, \code{logical}, or 
#'  \code{character} vectors, as for standard data frames.
#' @param keep.attrs logical flag or character vector specifying user-defined
#' 	attributes to preserve. This argument allows users to preserve attributes
#'  which are removed by the default subsetting methods. If \code{TRUE},
#'  all attributes are preserved, or none if \code{FALSE}. If it is a 
#' 	character vector, only the supplied attributes are kept. If a name is
#'  provided that does not correspond to an actual attribute, a warning is
#'  issued.
#' @param drop logical. If \code{TRUE} the result is coerced to the lowest 
#'  possible dimension, as for standard data frames.   
#'  If the dimensions are actually dropped, attribute \code{dimmeta} is also
#' 	dropped, losing all the metadata. 
#' @param value a suitable replacement value: it will be repeated a whole 
#'  number of times if necessary.	 
#' 
#' @seealso \link[base:Extract.data.frame]{[} operator for data frames.
#' @S3method `[` dframe

#  NOTE: Code copied from `[.data.frame`, then adapted.
#		 Wrapping around NextMethod() would be hard and extremely inefficient 
#			as out-of-bound rows can be selected (out-of-bound columns cannot)
#  	     Modified lines are marked with '##dframe'.

`[.dframe` <- function (x, i, j, keep.attrs = FALSE, 
		drop = if (missing(i)) TRUE else length(cols) == 1) {
	dm <- dimmeta(x)	## dframe
	hasDimmeta <- !is.null(dm)
    mdrop <- missing(drop)
	keptAttrs <- keptAttrs(x, keep.attrs, 
		c("class", "names", "row.names", "dimmeta"))		## dframe
    Narg <- nargs() - (!mdrop)  - (!missing(keep.attrs))	## dframe
    has.j <- !missing(j)
    if (Narg < 3L) {
        if (!mdrop) 
            warning("drop argument will be ignored")
        if (missing(i)) 
            return(x)
        if (is.matrix(i)) 
            return(as.matrix(x)[i])
        nm <- names(x)
        if (is.null(nm)) 
            nm <- character(0L)
        if (!is.character(i) && any(is.na(nm))) {
            names(nm) <- names(x) <- seq_along(x)
            #y <- NextMethod("[")
			y <- .Primitive("[")(as.data.frame(x), i)		## dframe
            cols <- names(y)
            if (any(is.na(cols))) 
                stop("undefined columns selected")
            cols <- names(y) <- nm[cols]
        }
        else {
			#y <- NextMethod("[")
			y <- .Primitive("[")(as.data.frame(x), i)		## dframe
            cols <- names(y)
            if (!is.null(cols) && any(is.na(cols))) 
                stop("undefined columns selected")
        }
        if (any(duplicated(cols))) 
            names(y) <- make.unique(cols)

		if (hasDimmeta)					## dframe
			dm[[2L]] <- metasubset(dm[[2L]], i, nm);
		result <- structure(y, class = oldClass(x), 
			row.names = .row_names_info(x, 0L), dimmeta = dm)
		if (length(keptAttrs))		## dframe
			attributes(result)[names(keptAttrs)] <- keptAttrs;
		return(result)
    }
    if (missing(i)) {
        if (missing(j) && drop && length(x) == 1L)			
            return(.subset2(x, 1L))
        nm <- names(x)
        if (is.null(nm)) 
            nm <- character(0L)
        if (!missing(j) && !is.character(j) && any(is.na(nm))) {
            names(nm) <- names(x) <- seq_along(x)
            y <- if (missing(j)) 
                x
            else .subset(x, j)
            cols <- names(y)
            if (any(is.na(cols))) 
                stop("undefined columns selected")
            cols <- names(y) <- nm[cols]
        }
        else {
            y <- if (missing(j)) 
                x
            else .subset(x, j)
            cols <- names(y)
            if (any(is.na(cols))) 
                stop("undefined columns selected")
        }
        if (drop && length(y) == 1L) 
            return(.subset2(y, 1L))
        if (any(duplicated(cols))) 
            names(y) <- make.unique(cols)
        nrow <- .row_names_info(x, 2L)
        if (drop && !mdrop && nrow == 1L) 
            return(structure(y, class = NULL, row.names = NULL))
        else {
			if (hasDimmeta)				## dframe
				dm[[2L]] <- metasubset(dm[[2L]], j, nm);			
			result <- structure(y, class = oldClass(x), 
				row.names = .row_names_info(x, 0L), dimmeta = dm)
			if (length(keptAttrs))	## dframe
				attributes(result)[names(keptAttrs)] <- keptAttrs;		
			return(result)
		}
    }
    xx <- x
    cols <- names(xx)
    x <- vector("list", length(x))
    x <- .Call("R_copyDFattr", xx, x, PACKAGE = "base")	
    oldClass(x) <- attr(x, "row.names") <- NULL
    if (!missing(j)) {
        nm <- names(x)
        if (is.null(nm)) 
            nm <- character(0L)
        if (!is.character(j) && any(is.na(nm))) 
            names(nm) <- names(x) <- seq_along(x)
        x <- x[j]
        cols <- names(x)
        if (drop && length(x) == 1L) {
            if (is.character(i)) {
                rows <- attr(xx, "row.names")
                i <- pmatch(i, rows, duplicates.ok = TRUE)
            }
            xj <- .subset2(.subset(xx, j), 1L)
            return(if (length(dim(xj)) != 2L) xj[i] else xj[i,, drop = FALSE])
        }
        if (any(is.na(cols))) 
            stop("undefined columns selected")
        if (!is.null(names(nm))) 
            cols <- names(x) <- nm[cols]		
        nxx <- structure(seq_along(xx), names = names(xx))
        sxx <- match(nxx[j], seq_along(xx))
		if (hasDimmeta)		## dframe
			dm[[2L]] <- metasubset(dm[[2L]], sxx, nm);			
    }
    else sxx <- seq_along(x)
	
    rows <- NULL
    if (is.character(i)) {
        rows <- attr(xx, "row.names")
        i <- pmatch(i, rows, duplicates.ok = TRUE)
    }
	if (hasDimmeta)		## dframe
		dm[[1L]] <- metasubset(dm[[1L]], i);			
	
    for (j in seq_along(x)) {
        xj <- xx[[sxx[j]]]
        x[[j]] <- if (length(dim(xj)) != 2L) 
            xj[i]
        else xj[i, , drop = FALSE]
    }
    if (drop) {
        n <- length(x)
        if (n == 1L) 
            return(x[[1L]])
        if (n > 1L) {
            xj <- x[[1L]]
            nrow <- if (length(dim(xj)) == 2L) 
                dim(xj)[1L]
            else length(xj)
            drop <- !mdrop && nrow == 1L
        }
        else drop <- FALSE
    }
    if (!drop) {
        if (is.null(rows)) 
            rows <- attr(xx, "row.names")
        rows <- rows[i]
        if ((ina <- any(is.na(rows))) | (dup <- any(duplicated(rows)))) {
            if (!dup && is.character(rows)) 
                dup <- "NA" %in% rows
            if (ina) 
                rows[is.na(rows)] <- "NA"
            if (dup) 
                rows <- make.unique(as.character(rows))
        }
        if (has.j && any(duplicated(nm <- names(x)))) 
            names(x) <- make.unique(nm)
        if (is.null(rows)) 
            rows <- attr(xx, "row.names")[i]
        attr(x, "row.names") <- rows
        oldClass(x) <- oldClass(xx)
    }
	attr(x, "dimmeta") <- dm		## dframe
	if (length(keptAttrs))		## dframe
		attributes(x)[names(keptAttrs)] <- keptAttrs;			
    x
}


