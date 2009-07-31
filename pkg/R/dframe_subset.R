################################################################################
# R PACKAGE:   dimmeta
# FILE:        R/darray_subset.R
# DESCRIPTION: Methods related to extraction and replacement of 
#			   parts of 'dframe' objects.  
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  2009 July 7
# LICENSE:     GPL-2
################################################################################

#' Extract or replace subsets of data frames with metadata along dimensions.
#' 
#' This method behaves as its \code{data.frame} 
#' \link[base:Extract.data.frame]{counterparts}, 
#' with the additional feature that the \code{dimmeta} attribute, when
#' present, is subsetted along with each of the corresponding dimension
#' subscripts.
#' 
#' @title Extract or Replace Parts of Data Frames with Metadata
#' @name Extract.dframe
#' @aliases Extract.dframe [.dframe [<-.dframe xpdrows.dframe
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

#' @name dframeReplace
#' @nord
#' @S3method `[<-` dframe

`[<-.dframe` <- function(x, i, j, value) {
    nA <- nargs()
    if (nA == 4L) {
        has.i <- !missing(i)
        has.j <- !missing(j)
    }
    else if (nA == 3L) {
        if (is.atomic(value)) 
            names(value) <- NULL
        if (missing(i) && missing(j)) {
            i <- j <- NULL
            has.i <- has.j <- FALSE
            if (is.null(value)) 
                return(x[logical(0L)])
        }
        else {
            if (is.logical(i) && is.matrix(i) && all(dim(i) == dim(x))) {
                nreplace <- sum(i, na.rm = TRUE)
                if (!nreplace) 
                  return(x)
                N <- length(value)
                if (N > 1L && N < nreplace && (nreplace%%N) == 0L) 
                  value <- rep(value, length.out = nreplace)
                if (N > 1L && (length(value) != nreplace)) 
                  stop("rhs is the wrong length for indexing by a logical matrix")
                n <- 0L
                nv <- nrow(x)
                for (v in seq_len(dim(i)[2L])) {
                  thisvar <- i[, v, drop = TRUE]
                  nv <- sum(thisvar, na.rm = TRUE)
                  if (nv) {
                    if (is.matrix(x[[v]])) 
                      x[[v]][thisvar, ] <- if (N > 1L) 
                        value[n + seq_len(nv)]
                      else value
                    else x[[v]][thisvar] <- if (N > 1L) 
                      value[n + seq_len(nv)]
                    else value
                  }
                  n <- n + nv
                }
                return(x)
            }
            if (is.matrix(i)) 
                stop("only logical matrix subscripts are allowed in replacement")
            j <- i
            i <- NULL
            has.i <- FALSE
            has.j <- TRUE
        }
    } else stop("need 0, 1, or 2 subscripts")
    if (has.j && length(j) == 0L) 
        return(x)
    cl <- oldClass(x)	
    class(x) <- NULL
    new.cols <- NULL
    nvars <- length(x)
    nrows <- .row_names_info(x, 2L)
    if (has.i) {
        rows <- NULL
        if (any(is.na(i))) 
            stop("missing values are not allowed in subscripted assignments of dframes")
        if (char.i <- is.character(i)) {
            rows <- attr(x, "row.names")
            ii <- match(i, rows)
            nextra <- sum(new.rows <- is.na(ii))
            if (nextra > 0L) {
                ii[new.rows] <- seq.int(from = nrows + 1L, length.out = nextra)
                new.rows <- i[new.rows]
            }
            i <- ii
        }
        if (all(i >= 0L) && (nn <- max(i)) > nrows) {
            if (is.null(rows)) 
                rows <- attr(x, "row.names")
            if (!char.i) {
                nrr <- (nrows + 1L):nn
                if (inherits(value, "data.frame") && (dim(value)[1L]) >= 
                  	length(nrr)) {
                  new.rows <- attr(value, "row.names")[seq_along(nrr)]
                  repl <- duplicated(new.rows) | match(new.rows, 
                    rows, 0L)
                  if (any(repl)) 
                    new.rows[repl] <- nrr[repl]
                }
                else new.rows <- nrr            
			}			
            x <- xpdrows.dframe(x, rows, new.rows)
            rows <- attr(x, "row.names")
            nrows <- length(rows)
        }
        iseq <- seq_len(nrows)[i]
        if (any(is.na(iseq))) 
            stop("non-existent rows not allowed")
    }
    else iseq <- NULL
    if (has.j) {
        if (any(is.na(j))) 
            stop("missing values are not allowed in subscripted assignments of data frames")
        if (is.character(j)) {
            if ("" %in% j) 
                stop("column name \"\" cannot match any column")
            jj <- match(j, names(x))
            nnew <- sum(is.na(jj))
            if (nnew > 0L) {
                n <- is.na(jj)
                jj[n] <- nvars + seq_len(nnew)
                new.cols <- j[n]
            }
            jseq <- jj
        }
        else if (is.logical(j) || min(j) < 0L) 
            jseq <- seq_along(x)[j]
        else {
            jseq <- j
            if (max(jseq) > nvars) {
                new.cols <- paste("V", seq.int(from = nvars + 
                  1L, to = max(jseq)), sep = "")
                if (length(new.cols) != sum(jseq > nvars)) 
                  stop("new columns would leave holes after existing columns")
                if (is.list(value) && !is.null(vnm <- names(value))) {
                  p <- length(jseq)
                  if (length(vnm) < p) 
                    vnm <- rep(vnm, length.out = p)
                  new.cols <- vnm[jseq > nvars]
                }
            }
        }
    }
    else jseq <- seq_along(x)
    if (any(duplicated(jseq))) 
        stop("duplicate subscripts for columns")
    n <- length(iseq)
    if (n == 0L) 
        n <- nrows
    p <- length(jseq)
    m <- length(value)
    if (!is.list(value)) {
        if (p == 1L) {
            N <- NROW(value)
            if (N > n) 
                stop(gettextf("replacement has %d rows, data has %d", 
                  N, n), domain = NA)
            if (N < n && N > 0L) 
                if (n%%N == 0L && length(dim(value)) <= 1L) 
                  value <- rep(value, length.out = n)
                else stop(gettextf("replacement has %d rows, data has %d", 
                  N, n), domain = NA)
            names(value) <- NULL
            value <- list(value)
        }
        else {
            if (m < n * p && (m == 0L || (n * p)%%m)) 
                stop(gettextf("replacement has %d items, need %d", 
                  m, n * p), domain = NA)
            value <- matrix(value, n, p)
            value <- split(value, col(value))
        }
        dimv <- c(n, p)
    }
    else {
        value <- unclass(value)
        lens <- sapply(value, NROW)
        for (k in seq_along(lens)) {
            N <- lens[k]
            if (n != N && length(dim(value[[k]])) == 2L) 
                stop(gettextf("replacement element %d is a matrix/data frame of %d rows, need %d", 
                  k, N, n), domain = NA)
            if (N > 0L && N < n && n%%N) 
                stop(gettextf("replacement element %d has %d rows, need %d", 
                  k, N, n), domain = NA)
            if (N > 0L && N < n) 
                value[[k]] <- rep(value[[k]], length.out = n)
            if (N > n) {
                warning(gettextf("replacement element %d has %d rows to replace %d rows", 
                  k, N, n), domain = NA)
                value[[k]] <- value[[k]][seq_len(n)]
            }
        }
        dimv <- c(n, length(value))
    }
    nrowv <- dimv[1L]
    if (nrowv < n && nrowv > 0L) {
        if (n%%nrowv == 0L) 
            value <- value[rep(seq_len(nrowv), length.out = n), 
                , drop = FALSE]
        else stop(gettextf("%d rows in value to replace %d rows", 
            nrowv, n), domain = NA)
    }
    else if (nrowv > n) 
        warning(gettextf("replacement data has %d rows to replace %d rows", 
            nrowv, n), domain = NA)
    ncolv <- dimv[2L]
    jvseq <- seq_len(p)
	
    if (ncolv < p) 
        jvseq <- rep(seq_len(ncolv), length.out = p)
    else if (ncolv > p) {
        warning(gettextf("provided %d variables to replace %d variables", 
            ncolv, p), domain = NA)
        new.cols <- new.cols[seq_len(p)]
    }
    if (length(new.cols)) {		
        nm <- names(x)
        rows <- .row_names_info(x, 0L)
        a <- attributes(x)
        a["names"] <- NULL
        x <- c(x, vector("list", length(new.cols)))
        attributes(x) <- a
        names(x) <- c(nm, new.cols)
        attr(x, "row.names") <- rows
		if (length(attr(x, "dimmeta")[[2L]]))		## dframe
			metalength(attr(x, "dimmeta")[[2L]]) <- 
				length(nm) + length(new.cols)					
    }	   
	
	if (has.i)	
        for (jjj in seq_len(p)) {
            jj <- jseq[jjj]
            vjj <- value[[jvseq[[jjj]]]]

            if (jj <= nvars) {
                if (length(dim(x[[jj]])) != 2L) 
                  x[[jj]][iseq] <- vjj
                else x[[jj]][iseq, ] <- vjj
            }
            else {
                x[[jj]] <- vjj[FALSE]
                if (length(dim(vjj)) == 2L) {
                  length(x[[j]]) <- nrows * ncol(vjj)
                  dim(x[[j]]) <- c(nrows, ncol(vjj))
                  x[[jj]][iseq, ] <- vjj
                }
                else {
                  length(x[[j]]) <- nrows
                  x[[jj]][iseq] <- vjj
                }
            }
        }
    else if (p > 0L) {
		isnull <- rep(FALSE, p)		## dframe
        for (jjj in p:1L) {
            jj <- jseq[jjj]			
            v <- value[[jvseq[[jjj]]]]	
            x[[jj]] <- v
			if (is.null(v))			## dframe
				isnull[jj] <- TRUE
            else if (is.atomic(x[[jj]])) 
                names(x[[jj]]) <- NULL
        }
		if (any(isnull) && length(cm <- attr(x, "dimmeta")[[2L]]))	## dframe
			attr(x, "dimmeta")[[2L]] <- metasubset(cm, -which(isnull))
	}
    if (length(new.cols) > 0L) {
        new.cols <- names(x)
        if (any(duplicated(new.cols))) 
            names(x) <- make.unique(new.cols)
    }
    class(x) <- cl
    x	
}

#' @nord
#' @seealso dframeHelpers
#  NOT EXPORTED

xpdrows.dframe <- function(x, old.rows, new.rows) {
    nc <- length(x)
    nro <- length(old.rows)
    nrn <- length(new.rows)
    nr <- nro + nrn
    for (i in seq_len(nc)) {
        y <- x[[i]]
        dy <- dim(y)
        cy <- oldClass(y)
        class(y) <- NULL
        if (length(dy) == 2L) {
browser()						
            dny <- dimnames(y)
            if (length(dny[[1L]]) > 0L) 
                dny[[1L]] <- c(dny[[1L]], new.rows)
            z <- array(y[1L], dim = c(nr, nc), dimnames = dny)
            z[seq_len(nro), ] <- y
            class(z) <- cy
            x[[i]] <- z
        } else {
            ay <- attributes(y)
            if (length(names(y)) > 0L) 
                ay$names <- c(ay$names, new.rows)
            length(y) <- nr
            attributes(y) <- ay
            class(y) <- cy
            x[[i]] <- y
        }
    }

    attr(x, "row.names") <- c(old.rows, new.rows)
	if (length(attr(x, "dimmeta")[[1L]]))		## dframe
		metalength(attr(x, "dimmeta")[[1L]]) <- nro + nrn 
    x
}
