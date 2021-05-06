#'@title Method for plotting class Rttest
#'
#'@description Makes different boxplots based on the object of class Rttest
#'
#'@details The function \code{myttest()} produces a list of class Rttest, which has three components(ttest, data, pop)
#'These are sourced from the object and used in \code{plot.Rttest()}
#'
#'
#' @param x object class Rttest
#' @param ... extra parameter options sent to the boxplot
#'
#' @return A boxplot of the two samples using \code{ggplot()} and \code{boxplot()}
#'
#' @importFrom ggplot2 ggplot aes ggtitle geom_boxplot
#' @importFrom graphics boxplot
#' @importFrom stats var.test t.test
#' @importFrom utils data
#'
#' @rdname plot.Rttest
#' @export
#'
#' @examples
#' obj <- myttest(x=rnorm(30,mean=10,sd=15), y=rnorm(30,mean=8,sd=15))
#' plot(obj)
plot.Rttest <- function(x,...)
{
    df = x$df
    pop = x[["df"]][["pop"]]
        #if test type is "To Sample t-text' make a boxplot
        if(x[['ttest']][['method']] == ' Two Sample t-test'){
            g <- ggplot(df, aes(x = pop, y = data)) +
                geom_boxplot(aes(fill = pop))
            g <- g + ggtitle(paste("P value =", round(x$ttest$p.value, 4),
                                   "Paired = ", x$paired))
            print(g)

            #if test type is 'Welch Two Sample t-test' make a different boxplot
        }else if(x[['ttest']][['method']] == 'Welch Two Sample t-test'){
            g <- ggplot(df, aes(x = pop, y = data)) +
                geom_boxplot(aes(fill = pop))
            g <- g + ggtitle(paste("P value =", round(x$ttest$p.value, 4),
                                   "Paired = ", x$paired))
            print(g)


            # # make a paired boxplot of difference between x and y
        }else{
            x2 = x[["df"]][["data"]][31:60] - x[["df"]][["data"]][1:30]
            g = boxplot(x2,
                        col = "skyblue",
                        main = "Difference between x and y",
                        xlab = "Difference x",
                        ylab = "sample")
            print(g)
        }

     }






