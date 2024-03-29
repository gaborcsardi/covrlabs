% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/transform.R
\name{transform_package}
\alias{transform_package}
\title{Transform all objects in a package}
\usage{
transform_package(package, skip = ".__NAMESPACE__.", closxp_callback = NULL)
}
\arguments{
\item{package}{Package name of the package to transform. If it is not
loaded, \code{transform_package} will load it.}

\item{skip}{Character vector of object names to skip.}

\item{closxp_callback}{If not \code{NULL}, then it must be a callback
fucntion to transform closures. It is called for every function, but
it is not called for functions nested into other functions.}
}
\description{
Transform all objects in a package
}
\seealso{
Other transform functions: 
\code{\link{transform_env}()},
\code{\link{transform_object}()}
}
\concept{transform functions}
