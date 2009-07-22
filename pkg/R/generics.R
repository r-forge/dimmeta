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
