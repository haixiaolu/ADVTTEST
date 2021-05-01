#'@title  A constructor function for t tests
#'
#'@description  Test the NULL hypothesis using t.test theory and make some boxplots
#'
#'@details  The constructor function using t.test theory to test the NULL hypothesis
#'and make a list that can be worked and further processed with an appropriate method(s)
#'
#' @param x vector of data
#' @param y vector of data
#' @param alpha numeric alpha level between 0, 1
#' @param paired logical value for paired parameter, TRUE or FALSE, default by FALSE
#'
#'
#' @return a list containing the data, and t test object
#'
#' @export
#'
#' @keywords ttest, t.test testing NHST student's t.test
#'
#' @seealso \code{\link{t.test}} for more information about t tests
#'
#' @examples
#'
#' myttest(x=rnorm(30,mean=10,sd=15), y=rnorm(30,mean=8,sd=15))

myttest <- function(x, y, alpha = 0.05, paired = FALSE)
{
    data <- vector(mode = "numeric", length = length(x) + length(y))
    pop <- vector(mode = "list", length = length(data))
    # Check if it's paired
    # if paried == FALSE, the use var.test
    if(paired == FALSE){
        vt= var.test(x, y)
        p = vt$p.value
    if(p > alpha){
        # # t test
        # if p value is greater than significance level alpha 0.05,
        # use the var.equal = True to test 'Two sample t-test'
        res = t.test(x, y, var.equal = TRUE, conf.level = 1-alpha)
    }else{ # if p value is less than alpha
        res = t.test(x, y, var.equal = FALSE, conf.level = 1 - alpha)
    }
    }else{ # it's paired, so check the length
        stopifnot(length(x) == length(y))
        res = t.test(x, y, paired = TRUE, conf.level = 1 - alpha)
    }

    # combine vector of x and y
    data = c(x, y)
    pop = rep(c("x", "y"), c(length(x), length(y)))
    df = data.frame("data" = data, "pop" = pop)
    # return a list and assign it to an objective
    obj = list(ttest = res, df = df, paired = paired)
    # the constructor class is called "myTest'
    class(obj) = "Rttest"
    obj
}






