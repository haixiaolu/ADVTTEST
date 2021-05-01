#' Method for print output object of class Rttest's components
#'
#'@description Print out components of t.test using an object of class Rttest
#'
#'@details The function \code{myttest()} produces a list of class Rttest, Which has three components,
#'(ttest, data, pop), in the components ttest, it contains all the statistics results, such as p value,
#'confidence interval, p.value, method, etc. These are sourced from the object and used in \code{print.Rttest()}
#'
#' @param x object class Rttest
#' @param ... extra parameter options sent to print method
#'
#'
#' @return Several statistic results from the object of class Rttest, include p.value, confidence interval,
#' test type, YES or NO to the NULL Hypothesis
#'
#' @rdname print.Rttest
#' @export
#'
#' @examples
#'set.seed(32); x=rnorm(30,mean=10,sd=5)
#'set.seed(35); y=rnorm(30,mean=8,sd=15)
#'ans2=myttest(x,y,alpha=0.05,paired=FALSE)
#'print(ans2)
#'
print.Rttest <- function(x,...){
    # print out the p value
    `p.value` = x[['ttest']][['p.value']]
    # output interval
    `confidence Interval` = x[['ttest']][["conf.int"]]
    # output t.test type
    `method` = x[['ttest']][['method']]
    `Y/N`  = ifelse(x[['ttest']][['p.value']] <= 0.05, "Yes, reject NULL", "NO, not reject NULL")
    # return a list of p value, interval and test type
    list("p_value:", `p.value`,
         "CI:",
         `confidence Interval`,
         "Y/N", `Y/N`,
         "test_type:",`method`)
}

