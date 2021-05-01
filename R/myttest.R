#' myttest function
#'
#' Test the NULL hypothesis using t.test theory and make some boxplots
#'
#' @param x numeric x value
#' @param y numeric y value
#' @param alpha numeric alpha value
#' @param paired logical value for paired parameter, TRUE or FALSE
#'
#'
#' @return a list of different t.test type, p value
#' and boxplots for different test type
#' @export
#'
#' @examples
#'
#' set.seed(32); x=rnorm(30,mean=10,sd=15)
#' set.seed(35); y=rnorm(30,mean=8,sd=15)
#' ans1=myttest(x,y,alpha=0.05,paired=FALSE)
#' print(ans1)
#' class(ans1)
#' plot(ans1)
#
myttest <- function(x, y, alpha, paired = FALSE)
{
    # Check if it's paired value
    # if paried == TRUE, the use t.test when paried = TRUE
    if(paired != FALSE){
        res = t.test(y, x, paired = TRUE)
    }else{
        v = var.test(x, y)
        p = v$p.value
        # # t test
        # if p value is greater than significance level alpha 0.05,
        # use th var.equal = True to test 'Two sample t-test'
        if(p >= alpha){
            res = t.test(y, x, var.equal = TRUE)
        }else{
            res = t.test(y, x, var.equal = FALSE)
        }
    }
    # return a list and assign it to an objective
    obj = list(data = list(x = x, y = y, res = res))
    # the constructor class is called "myTest'
    class(obj) = "Rttest"
    obj

}


# print method
print.Rttest <- function(x){
    # print out the p value
    `p.value` = x[['data']][['res']][['p.value']]
    # output interval
    `confidence Interval` = x[['data']][['res']][["conf.int"]]
    # output t.test type
    `method` = x[['data']][['res']][['method']]
    `Y/N`  = ifelse(x[['data']][['res']][['p.value']] <= 0.05, "Yes, reject NULL", "NO, not reject NULL")
    # return a list of p value, interval and test type
    list("p_value:", `p.value`,
         "CI:",
         `confidence Interval`,
         "Y/N", `Y/N`,
         "test_type:",`method`)
}


plot.Rttest <- function(x)
{
            # data of x and y
            x1 = x[['data']][['x']]
            y = x[['data']][['y']]
            # the difference between x and y
            x2 = y - x1
            # boxplot
            # population
            pop = rep(c("x1", "y"), c(length(x1), length(y)))

            # make a data frame
            df = data.frame(pop = pop,
                            samples = c(x1, y))

            #if(!require(ggplot2)) install.packages("ggplot2")
            library(ggplot2)
            # if test type is "To Sample t-text' make a boxplot
            if(x[['data']][['res']][['method']] == ' Two Sample t-test'){
                g = ggplot(df, aes(x1 = pop,
                                   y = samples,
                                   fill = pop)) +
                    geom_boxplot()
                print(g)
            # if test type is 'Welch Two Sample t-test' make a different boxplot
            }else if(x[['data']][['res']][['method']] == 'Welch Two Sample t-test'){
                g = ggplot(df, aes(x1 = pop,
                                   y = samples,
                                   fill = pop)) +
                    geom_boxplot()
                print(g)
            # make a paired boxplot of difference between x and y
            }else{
                    # g = ggplot(data = df, aes(x = x2, y = samples)) +
                    #     geom_boxplot()
                    g = boxplot(x2,
                                col = "skyblue",
                                main = "Difference between x and y",
                                xlab = "Difference x",
                                ylab = "sample")

                    print(g)
            }

}



