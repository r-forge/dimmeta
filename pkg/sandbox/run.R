################################################################################
# R PACKAGE:   dimmeta
# FILE:        temp/run.R
# DESCRIPTION: Copy/paste code to facilitate testing & debugging sessions. 
# AUTHOR:      Enrique Bengoechea <enrique.bengoechea@credit-suisse.com>
# CREATED ON:  28/05/2009
################################################################################
stop("not to be directly sourced!"); 

library(DevTools);
useDevLib();
devPath(file.path("C:", "ebe", "Work", "Projects", "PaRiS_R", "dimmeta"));

#library(ppTools)

s("generics.R")
s("methods_default.R")
s("darray.R")
s("darray_subset.R")

s("mframe.R")
s("mframe_subset.R")
s("mvector.R")
s("mvector_subset.R")

# For testing namespace, etc. without loading DevTools
.libPaths(c("C:\\PROGRA~1\\R\\R-28~1.1\\dev-library", .libPaths()))

# Runs all unit tests in 'inst/unitTests'
ut();


