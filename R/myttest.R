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
    if(paired != FALSE){
        res = t.test(y, x, paired = TRUE)
    }else{
        v = var.test(x, y)
        p = v$p.value
        # # t test
        # if p value is greater than significance level alpha 0.05,
        # no significant difference between the variances of the two sets
        if(p >= alpha){
            res = t.test(y, x, var.equal = TRUE)
        }else{
            res = t.test(y, x, var.equal = FALSE)
        }
    }
    obj = list(data = list(x = x, y = y, res = res))
    class(obj) = "myTest"
    obj

}


# print method
print.myTest <- function(x){
    `p.value` = x[['data']][['res']][['p.value']]
    `confidence Interval` = x[['data']][['res']][["conf.int"]]
    `method` = x[['data']][['res']][['method']]
    list("p_value:", `p.value`,
         "CI:",
         `confidence Interval`,
         "test_type:",`method`)
}


plot.myTest <- function(x)
{



            x1 = x[['data']][['x']]
            y = x[['data']][['y']]
            x2 = y - x1
            # boxplot
            # population
            pop = rep(c("x1", "y"), c(length(x1), length(y)))

            # make a data frame
            df = data.frame(pop = pop,
                            samples = c(x1, y))

            #if(!require(ggplot2)) install.packages("ggplot2")
            library(ggplot2)
            if(x[['data']][['res']][['method']] == ' Two Sample t-test'){
                g = ggplot(df, aes(x = pop,
                                   y = samples,
                                   fill = pop)) +
                    geom_boxplot()
                print(g)
            }else if(x[['data']][['res']][['method']] == 'Welch Two Sample t-test'){
                g = ggplot(df, aes(x = pop,
                                   y = samples,
                                   fill = pop)) +
                    geom_boxplot()
                print(g)

            }else{
                    g = boxplot(x2,
                                col = "skyblue",
                                main = "Difference between x and y",
                                xlab = "Difference x",
                                ylab = "sample")

                    print(g)
            }

}



