% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotRttest.R
\name{plot.Rttest}
\alias{plot.Rttest}
\title{Method for plotting class Rttest}
\usage{
\method{plot}{Rttest}(x, ...)
}
\arguments{
\item{x}{object class Rttest}

\item{...}{extra parameter options sent to the boxplot}
}
\value{
A boxplot of the two samples using \code{ggplot()} and \code{boxplot()}
}
\description{
Makes different boxplots based on the object of class Rttest
}
\details{
The function \code{myttest()} produces a list of class Rttest, which has three components(ttest, data, pop)
These are sourced from the object and used in \code{plot.Rttest()}
}
\examples{
obj <- myttest(x=rnorm(30,mean=10,sd=15), y=rnorm(30,mean=8,sd=15))
plot(obj)
}
