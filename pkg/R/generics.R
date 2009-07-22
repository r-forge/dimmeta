################################################################################
# R PACKAGE:   metadata
# FILE:        R/generics.R
# DESCRIPTION: Generic functions introduced in this package. 
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  2009 July 7
# LICENSE:     GPL-2
################################################################################

#' @nord
#' @export

as.marray <- function(x, ...) UseMethod("as.marray");

#' @nord
#' @export

dimdata <- function(x, ...) UseMethod("dimdata");

#' @name dimdataReplace
#' @nord
#' @export `dimdata<-`

`dimdata<-` <- function(x, value) UseMethod("dimdata<-");

#' @nord
#' @export

rowdata <- function(x, ...) UseMethod("rowdata");

#' @name rowdataReplace
#' @nord
#' @export `rowdata<-`

`rowdata<-` <- function(x, value) UseMethod("rowdata<-");

#' @nord
#' @export

coldata <- function(x, ...) UseMethod("coldata");

#' @name coldataReplace
#' @nord
#' @export `coldata<-`

`coldata<-` <- function(x, value) UseMethod("coldata<-");
