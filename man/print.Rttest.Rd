% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/printRttest.R
\name{print.Rttest}
\alias{print.Rttest}
\title{Method for print output object of class Rttest's components}
\usage{
\method{print}{Rttest}(x, ...)
}
\arguments{
\item{x}{object class Rttest}

\item{...}{extra parameter options sent to print method}
}
\value{
Several statistic results from the object of class Rttest, include p.value, confidence interval,
test type, YES or NO to the NULL Hypothesis
}
\description{
Print out components of t.test using an object of class Rttest
}
\details{
The function \code{myttest()} produces a list of class Rttest, Which has three components,
(ttest, data, pop), in the components ttest, it contains all the statistics results, such as p value,
confidence interval, p.value, method, etc. These are sourced from the object and used in \code{print.Rttest()}
}
\examples{
set.seed(32); x=rnorm(30,mean=10,sd=5)
set.seed(35); y=rnorm(30,mean=8,sd=15)
ans2=myttest(x,y,alpha=0.05,paired=FALSE)
print(ans2)

}
