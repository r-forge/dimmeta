################################################################################
# R PACKAGE:   dimmeta
# FILE:        R/generics.R
# DESCRIPTION: Generic functions introduced in this package. 
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  2009 July 7
# LICENSE:     GPL-2
################################################################################

#' @nord
#' @export

as.darray <- function(x, ...) UseMethod("as.darray");

#' @nord
#' @export

as.dframe <- function(x, ...) UseMethod("as.dframe");

#' @nord
#' @export

dimmeta <- function(x, ...) UseMethod("dimmeta");

#' @name dimmetaReplace
#' @nord
#' @export `dimmeta<-`

`dimmeta<-` <- function(x, value) UseMethod("dimmeta<-");

#' @nord
#' @export

rowmeta <- function(x, ...) UseMethod("rowmeta");

#' @name rowmetaReplace
#' @nord
#' @export `rowmeta<-`

`rowmeta<-` <- function(x, value) UseMethod("rowmeta<-");

#' @nord
#' @export

colmeta <- function(x, ...) UseMethod("colmeta");

#' @name colmetaReplace
#' @nord
#' @export `colmeta<-`

`colmeta<-` <- function(x, value) UseMethod("colmeta<-");

#' @name unmeta
#' @nord
#' @export 

unmeta <- function(x, ...) UseMethod("unmeta");

#' Generic function used by dimension-sensitive metadata handling methods
#' to find out or modify the size of the metadata associated to the data object. 
#' 
#' The default setter method returns 0 when \code{x} is \code{NULL},
#' \code{length(x)} when \code{x} is an object without dimensions (such as a
#' vector), and \code{dim(x)[1]} when \code{x} has a non-null
#' \code{dim} attribute (e.g. arrays or data frames).
#' 
#' The replacement method is used mainly for extending the metadata when
#' new items are added to the associated data object, filling new elements
#' with adequate empty values (\code{NA}s, \code{NULL}s....)
#' 
#' New methods can be added for user-defined classes used for dimension
#' metadata. 
#'  
#' @title Length of Dimension Metadata along the Data Object Extent
#' @param x an object used to store dimension-sensitive metadata 
#'  
#' @return \code{metalength} returns an integer number. 
#' 	\code{metalength<-} is invoked by its side effect of modifying the object
#' 	length. 
#' @export

metalength <- function(x) UseMethod("metalength");

#' @name metalengthReplace
#' @nord
#' @export

`metalength<-` <- function(x, value) UseMethod("metalength<-");

#' Generic function used to assign names to dimension-sensitive metadata
#' along the extent associated to the data object.
#' 
#' The default method assign names using \code{names<-} when \code{x} is 
#' an object without dimensions (such as a vector), and 
#' \code{dimnames(x)[1]<-} when \code{x} has a non-null
#' \code{dim} attribute (e.g. arrays or data frames).
#' 
#' New methods can be added for user-defined classes used for dimension
#' metadata. 
#'  
#' @title Assign Names to Dimension Metadata
#' @name metanamesReplace
#' @param x an object used to store dimension-sensitive metadata
#' @param value a character vector of names. 
#'  
#' @export `metanames<-`

`metanames<-` <- function(x, value) UseMethod("metanames<-");

#' Generic function invoked by \code{\link{[.darray}} and 
#' \code{[.dframe} to subset dimension-sensitive metadata.
#' 
#' The default method returns \code{NULL} when \code{x} is \code{NULL},
#' \code{x[i]} when \code{x} is an object without dimensions (such as a
#' vector), and \code{x[i,,drop=FALSE]} when \code{x} has a non-null
#' \code{dim} attribute (e.g. arrays or data frames).
#' 
#' New methods can be added for user-defined classes used for dimension
#' metadata. 
#'  
#' @title Extract Subsets of Dimension Metadata
#' @param x an object used to store dimension-sensitive metadata 
#' @param i a subscript used to index \code{x}. 
#' @param names if \code{i} is a character vector, \code{names} is the
#' 	character vector of names of \code{x} along the corresponding dimension,
#' 	so that \code{i} can be matched againtst them.
#' @param exact logical flag. If \code{TRUE} and name matching is used
#' 	(when \code{i} is a character vector), uses \link{match}{exact} string 
#' 	matching. When \code{FALSE}, uses \link{pmatch}{partial} matching.
#' @param rm logical flag. If \code{TRUE}, the \code{i} index is interpreted
#' 	as subscripting the elements to remove from \code{x}. Defaults to 
#'  \code{FALSE}. 
#'  
#' @return A subset of \code{x}. 
#' @export

metasubset <- function(x, i, names, exact, rm) UseMethod("metasubset");

#' Generic function invoked by \code{\link{rbind.darray}} and 
#' \code{cbind.darray} to bind dimension metadata along the dimension
#' being combined. New methods can be added, allowing developers to
#' use specialized classes for the metadata. 
#' 
#' @title Combine Dimension Metadata
#' @aliases metabind metabind.default metabind.list metabind.matrix
#' 	metabind.data.frame 
#' @usage metabind(model, meta, lengths)
#' 
#' 	\method{metabind}{default}(model, meta, lengths)
#' 	\method{metabind}{list}(model, meta, lengths)
#' 	\method{metabind}{matrix}(model, meta, lengths)
#' 	\method{metabind}{data.frame}(model, meta, lengths)
#' @param model the metadata object to be used as the combine "model". 
#' @param meta list of objects representing dimension metadata. Normally,
#' 	several elements will be \code{NULL}, corresponding to objects being
#' 	combined by \code{rbind} or \code{cbind} that do not have 
#' 	dimension-sensitive metadata. 
#' @param lengths integer vector of the same length as \code{meta} that
#'  supplies the number of elements that should be added by each element
#'  of \code{meta} to the final (combined) result. 
#' 
#' @return An object representing dimension metadata for a set of objects
#' 	that are combined using \code{rbind.array} or \code{cbind.array}.
#'  Normally it will have the same class as \code{model}, but this is not
#' 	required.   
#' @seealso \code{\link{rbind.darray}}.
#' @export

metabind <- function(model, meta, lengths) UseMethod("metabind");
