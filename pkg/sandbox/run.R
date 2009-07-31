################################################################################
# R PACKAGE:   dimmeta
# FILE:        temp/run.R
# DESCRIPTION: Copy/paste code to facilitate testing & debugging sessions. 
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  28/05/2009
################################################################################
stop("not to be directly sourced!"); 

library(DevTools);
devPkg("dimmeta");

#useDevLib();
#library(ppTools)
library(abind)

s("generics.R")
s("methods_default.R")
s("darray.R")
s("darray_subset.R")
s("darray_bind.R")
s("dframe.R")
s("dframe_subset.R")
s("dframe_bind.R")

# For testing namespace, etc. without loading DevTools
.libPaths(c("C:\\PROGRA~1\\R\\R-28~1.1\\dev-library", .libPaths()))

# Runs all unit tests in 'inst/unitTests'
ut();


