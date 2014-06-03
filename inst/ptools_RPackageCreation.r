# ptools_RPackageCreation.r
# Don Boyd
# 6/3/2014

# http://www.molecularecologist.com/2013/11/using-github-with-r-and-rstudio/
# https://www.rstudio.com/ide/docs/version_control/overview
# http://kbroman.github.io/github_tutorial/

# http://jeromyanglim.tumblr.com/post/63159223332/how-to-push-existing-git-rstudio-repository-to-github

# from RStudio/Tools/Shell... execute the following to set the link to github up:
#   git remote add origin https://github.com/donboyd5/ptools.git
#   git push ptools 

# http://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/
# http://adv-r.had.co.nz/Package-basics.html
# https://github.com/klutometis/roxygen#roxygen2
# http://www.rstudio.com/ide/docs/packages/overview

# http://cran.r-project.org/doc/manuals/R-exts.html#Data-in-packages
# https://sites.google.com/site/hackoutwiki/developers-corner/developing-r-packages

# The following SSH key was added to your account:
#   
#   GitHub for Windows - Don-PC
# 75:4b:6d:82:41:88:79:28:e1:b2:7a:01:66:93:61:8f

# 
# If you believe this key was added in error, you can remove the key and disable
# access at the following location:
#   
#   https://github.com/settings/ssh

# getOption("defaultPackages")

# http://rstudio-pubs-static.s3.amazonaws.com/628_79ab370204dd4b8a85ebbcde8ef95998.html

# install.packages("devtools")
# find_rtools()
# devtools::install_github("klutometis/roxygen")
# devtools::install_github("yihui/roxygen2") # I installed from CRAN

library(devtools)
library(roxygen2)

setwd("E:\\R\\GitHub\\")
# create("btools") only run this when needed

# then, create the functions and get into the R subdirectory for the package


# REPEAT the steps below each time the package is revised
# documentation (must be in the r files with functions)
# setwd("./ptools")
# document()
devtools::document(pkg="E:\\R\\GitHub\\ptools", clean=TRUE, roclets=c("rd", "namespace", "collate")) # roclets default seems to be c("rd", "namespace")
devtools::document(pkg="E:\\R\\GitHub\\ptools", clean=TRUE, roclets=c("rd")) # roclets default seems to be c("rd", "namespace")
# 
# getwd()
# # install
# setwd("..")
# install("ptools")


# optionally install from github
# setwd("..")
devtools::install_github("ptools","donboyd5")




# remove.packages("btools")


# here is a good example of how to document a function

#' @title Sum of Vector Elements
#'
#' @description
#' \code{sum} returns the sum of all the values present in its arguments.
#'
#' @details
#' This is a generic function: methods can be defined for it directly
#' or via the \code{Summary} group generic.  For this to work properly,
#' the arguments \code{...} should be unnamed, and dispatch is on the
#' first argument.'
sum <- function(..., na.rm = TRUE) {}





