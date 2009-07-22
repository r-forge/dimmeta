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

`unmeta<-` <- function(x, ...) UseMethod("unmeta");

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
