---
title: "ADVTTEST Package"
author: "Haixiao Lu"
date: "2021-05-03 21:58:25"
output:
  rmarkdown::html_vignette
vignette: >
  %\VignetteIndexEntry{myvignette} 
  %\VignetteEngine{knitr::knitr} 
  %\VignetteEncoding{UTF-8}
---




```r
library(ADVTTEST)
library(ggplot2)
```


# Introduction

The package `ADVTTEST` is made based on the ideas of `t.test` theory to test the NULL hypothesis
that the underlying means of two populations. The purpose of this package is to use OOP `S3` approach --
which is the most commonly used object oriented form in R that studied through **Advanced R** class. 

# Function

## Constructor

The first function is called `myttest` and it uses several t.test type to test the NULL hypothesis, 
and it produces a list of useful information to be processed by method functions, such as `p.value`,
`Confindence Interval`, `method`, etc.


```r
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
```


### Check out the output class


```r
set.seed(32); x=rnorm(30,mean=10,sd=15)
set.seed(35); y=rnorm(30,mean=8,sd=15)
ans1=myttest(x,y,alpha=0.05,paired=FALSE)
class(ans1)
#> [1] "Rttest"
```

You can see the output is of class `Rttest`


The function returns a list of components. The components then be operated on by an appropriate 
method attached to a generic. If you want to check the output components, try follow `code`

```r
names(ans1)
#> [1] "ttest"  "df"     "paired"
```


## Methods

### Print method

The `print` method is attached to the generic function `print()`. This method simply print out the 
statistic summary information from the output components. 


```r
print.Rttest <- function(x){
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
```


We can call it by simply invoking the `print()` function

```r
print(ans1)
#> [[1]]
#> [1] "p_value:"
#> 
#> [[2]]
#> [1] 0.8249868
#> 
#> [[3]]
#> [1] "CI:"
#> 
#> [[4]]
#> [1] -7.623130  6.100201
#> attr(,"conf.level")
#> [1] 0.95
#> 
#> [[5]]
#> [1] "Y/N"
#> 
#> [[6]]
#> [1] "NO, not reject NULL"
#> 
#> [[7]]
#> [1] "test_type:"
#> 
#> [[8]]
#> [1] " Two Sample t-test"
```

### Plot method

The `plot` method is attached to the generic function `plot()`. This method is plotting the 
output components that we defined in the `plot.Rttest()` function. 


```r
plot.Rttest <- function(x)
{
            df = x$df
            #if test type is "To Sample t-text' make a boxplot
            if(!requireNamespace("ggplot2", quietly = TRUE)){
              stop("Package \"ggplot2\" needed for this function to work. 
                     Please install it.",
                    call. = FALSE)
              if(x[['ttest']][['method']] == ' Two Sample t-test'){
                g <-ggplot2:: ggplot(df, aes(x = pop, y = data)) +
                    geom_boxplot(aes(fill = pop))
                g <- g + ggtitle(paste("P value =", round(x$ttest$p.value, 4),
                                       "Paired = ", x$paired))
                print(g)

            #if test type is 'Welch Two Sample t-test' make a different boxplot
            }else if(x[['ttest']][['method']] == 'Welch Two Sample t-test'){
                g <- ggplot2::ggplot(df, aes(x = pop, y = data)) +
                    geom_boxplot(aes(fill = pop))
                g <- g + ggtitle(paste("P value =", round(x$ttest$p.value, 4),
                                       "Paired = ", x$paired))
                print(g)
            # # make a paired boxplot of difference between x and y
            }else{
                    x2 = x[["df"]][["data"]][31:60] - x[["df"]][["data"]][1:30]
                    # g = ggplot(data = df, aes(x = x2, y = data)) +
                    #     geom_boxplot()
                    g = boxplot(x2,
                                col = "skyblue",
                                main = "Difference between x and y",
                                xlab = "Difference x",
                                ylab = "sample")
                    print(g)
             }
              
            }
            

}
```


We can call it by simply invoking the `plot()` function

```r
plot(ans1)
```


# Shiny Web Application

Shiny is an R package that makes it easy to build interactive web apps straight from R. 
There are three basic parts in the Shiny app. 

* ui --> which is to display the interactive user interface. 
* server --> which take user's input and produce an output for user
* shinyApp --> which run the function that contains both ui and server


## part 1  `ui`

```r
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
        titlePanel("Testing the NULL Hypothesis"),
        sidebarLayout(position = "right",
            sidebarPanel(
                selectInput('xcol', 'X variable', c('x', 'x1', 'x2')),
                selectInput('ycol', 'Y variable', c("y", "y1", "y2")),
                checkboxInput(inputId = "summary_res",
                              label = strong("Show Summary Stats"),
                              value = TRUE)
            ),

        # Show a plot of the generated distribution
            mainPanel(
               plotOutput(outputId = "boxPlot", height = "400px"),
               # Display this only if the summary is shown
                h4("Summary"),
                verbatimTextOutput("summary")
            )
    )
 )

```


For part `ui`, it's a interactive user interface which display on the web browsers. 
For this shiny app, it contains three parts in the `ui` structure:

* `titlePanel()` -- which display the title of the app
* `sidebarPanel()` -- which display the options for user to choose (user input)
* `mainPanel()` -- which display the results, either could be graphs or texts.

For this section in this particular app,

* it will display the title of the app. 
* And it allows use to choose which variables `(x, x1, x2, y, y1, y2)` they want to calculate. 
* It will display a boxplot and statistic summary information based on the user input. 


## part 2 `server`


```r
# Define server logic required to draw a histogram
server <- function(input, output) {

    selectedData <- reactive({
        df[,c(input$xcol, input$ycol)]
    })

    output$boxPlot <- renderPlot({

        pop = rep(c("x", "y"), c(length(df$x), length(df$y)))
        # make a data frame
        df1 = data.frame(pop = pop,
                        samples = c(df$x, df$y))
        if(!requireNamespace("ggplot2", quietly = TRUE)){
          stop("Package \"ggplot2\" needed for this function to work. Please install it.",
             call. = FALSE)
           # option for input 1
        if ((input$xcol == 'x') && (input$ycol == 'y')){
            p = ggplot(df1, aes(x=pop,
                                y = samples,
                                fill = pop)) +
                        geom_boxplot() +
                        ggtitle("Boxplot for x and y")

            # add stype to the title
            p + theme(plot.title = element_text(color = '#993333',
                                                size = 20,
                                                face='bold'))

        # option for input 2
        }else if((input$xcol == 'x') && (input$ycol == 'y1')){
            pop = rep(c("x", "y1"), c(length(df$x), length(df$y1)))
            df3 = data.frame(pop = pop,
                             samples = c(df$x, df$y1))

            p = ggplot(df3,aes(x = pop,
                               y = samples,
                               fill = pop)) +
                geom_boxplot() +
                ggtitle("Boxplot for x and y1")
            p + theme(plot.title = element_text(color = 'blue',
                                                size = 20,
                                                face='bold'))

        # option for input 3
        }else if((input$xcol == 'x') && (input$ycol == 'y2')){
               `Difference of X` = df$y2 - df$x
                boxplot(`Difference of X`,
                        col = "#377EB8",
                        main = "Difference between x and y",
                        xlab = "Difference x",
                        ylab = "sample")

        # option for input 4
        }else if((input$xcol == 'x1') && (input$ycol == 'y')){
                pop = rep(c("x1", "y"), c(length(df$x1), length(df$y)))
                df4 = data.frame(pop = pop,
                                samples = c(df$x1, df$y))
                p = ggplot(df4,aes(x = pop,
                                   y = samples,
                                   fill = pop)) +
                                geom_boxplot() +
                                ggtitle("Boxplot for x1 and y")

                p + theme(plot.title = element_text(color = 'red',
                                                    size = 20,
                                                    face='bold'))

        # option for input 5
        }else if((input$xcol == 'x1') && (input$ycol == 'y1')){
            pop = rep(c("x1", 'y1'), c(length(df$x1), length(df$y1)))
            # make a data frame for input 2
            df2 = data.frame(pop = pop,
                             samples = c(df$x1, df$y1))

            p = ggplot(df2,aes(x = pop,
                               y = samples,
                               fill = pop)) +
                        geom_boxplot() +
                        ggtitle("Boxplot for x1 and y1")

            # add style to the title
            p + theme(plot.title = element_text(color = 'blue',
                                                size = 20,
                                                face='bold'))
        # option for input 6
        }else if((input$xcol == "x1") && (input$ycol == "y2")){
            `Difference of X1` = df$y2 - df$x1
             boxplot(`Difference of X1`,
                     col = "#4DAF4A",
                     main = "Difference between x1 and y",
                     xlab = "Difference x",
                     ylab = "sample")

        # option for input 7
        }else if((input$xcol == "x2") && (input$ycol == "y")){
            pop = rep(c("x2", "y"), c(length(df$x2), length(df$y)))
            df5 = data.frame(pop = pop,
                             samples = c(df$x2, df$y))

            p = ggplot(df5,aes(x = pop,
                               y = samples,
                               fill = pop)) +
                     geom_boxplot() +
                     ggtitle("Boxplot for x2 and y")

            p + theme(plot.title = element_text(color = '#984EA3',
                                                size = 20,
                                                face='bold.italic'))

         # option for input 8
        }else if((input$xcol == "x2") && (input$ycol == "y1")){
            pop = rep(c("x2", "y1"), c(length(df$x2), length(df$y1)))
            df5 = data.frame(pop = pop,
                             samples = c(df$x2, df$y1))

            p = ggplot(df5,aes(x = pop,
                               y = samples,
                               fill = pop)) +
                        geom_boxplot() +
                        ggtitle("Boxplot for x2 and y1")

            p + theme(plot.title = element_text(color = 'skyblue',
                                                size = 20,
                                                face='bold.italic'))

         # option for input 9
        }else{
            `Difference of X2` = df$y2 - df$x2
             boxplot(`Difference of X2`,
                     col = "#FF7F00",
                     main = "Difference between x2 and y",
                     xlab = "Difference x",
                     ylab = "sample")
        }
        }
        

    })
    output$summary <- renderPrint({
        if((input$xcol == "x") && (input$ycol == "y2")){
            x = df$y2 - df$x
            res = t.test(y, x, paired = TRUE)
            res
        }else if((input$xcol == "x1") && (input$ycol == "y2")){
            x = df$y2 - df$x1
            res = t.test(y, x, paired = TRUE)
            res
        }else if((input$xcol == "x2") && (input$ycol == "y2")){
            x = df$y2 - df$x2
            res = t.test(y, x, paired = TRUE)
            res
        }else if((input$xcol == "x") && (input$ycol == "y")){
            v = var.test(df$x, df$y)
            p = v$p.value
            if(p >= 0.05){
                res = t.test(y, x, var.equal = TRUE)
                res
            }else{
                res = t.test(y, x, var.equal = FALSE)
                res
            }

        }else if((input$xcol == "x") && (input$ycol == "y1")) {
            v = var.test(df$x, df$y1)
            p = v$p.value
            if(p >= 0.05){
                res = t.test(y1, x, var.equal = TRUE)
                res
            }else{
                res = t.test(y1, x, var.equal = FALSE)
                res
            }

        }else if((input$xcol == "x1") && (input$ycol == "y")){
            v = var.test(df$x1, df$y)
            p = v$p.value
            if(p >= 0.05){
                res = t.test(y, x1, var.equal = TRUE)
                res
            }else{
                res = t.test(y, x1, var.equal = FALSE)
                res
            }
        }else if((input$xcol == "x1") && (input$ycol == "y1")){
            v = var.test(df$x1, df$y1)
            p = v$p.value
            if(p >= 0.05){
                res = t.test(y1, x1, var.equal = TRUE)
                res
            }else{
                res = t.test(y1, x1, var.equal = FALSE)
                res
            }
        }else if((input$xcol == "x2") && (input$ycol == "y")){
            v = var.test(df$x2, df$y)
            p = v$p.value
            if(p >= 0.05){
                res = t.test(y, x2, var.equal = TRUE)
                res
            }else{
                res = t.test(y, x2, var.equal = FALSE)
                res
            }
        }else if((input$xcol == "x2") && (input$ycol == "y1")){
            v = var.test(df$x2, df$y1)
            p = v$p.value
            if(p >= 0.05){
                res = t.test(y1, x2, var.equal = TRUE)
                res
            }else{
                res = t.test(y1, x2, var.equal = FALSE)
                res
            }
        }
    })
}
```

For the `server` section, it will only do one thing, that is based on the user inputs from the `ui` section, do some calculation behind scene and produce some results that defined in the `mainPanel()` 
in the `ui` section. 

For this particular app, the `server` will run some function in logical order to produce the boxplot
and statistic summary information based on the user inputs. 

Here's the process of the `server`:

* User can choose a `X` variable in `(x, x1, x2)`, `Y` variable in `(y, y1, y2)` --> (inputs)
* Then based on users' inputs, the `server` will run the specific line of codes to produce the graph
* It also run other specific line of codes to produce the statistic summary information based `inputs`


## part 3 `shinyApp` 

Then the third part of the app is that we place both 
`ui` and `server` functions into the `shinyApp()` function

```r
# Run the application
shinyApp(ui = ui, server = server)
```


# Shiny Server

We put `ui` and `server` into a shiny app and can be invoked using:


```r
shinyboxplot<-function(){
    shiny::runApp(system.file("inst/shiny1/myTtest", package = "ADVTTEST"),launch.browser = TRUE)
}
```


# Demonstration the package

## Sample 1

```r
set.seed(32); x=rnorm(30,mean=10,sd=15)
set.seed(35); y=rnorm(30,mean=8,sd=15)
ans1=ADVTTEST::myttest(x,y,alpha=0.05,paired=FALSE)
print(ans1)
#> [[1]]
#> [1] "p_value:"
#> 
#> [[2]]
#> [1] 0.8249868
#> 
#> [[3]]
#> [1] "CI:"
#> 
#> [[4]]
#> [1] -7.623130  6.100201
#> attr(,"conf.level")
#> [1] 0.95
#> 
#> [[5]]
#> [1] "Y/N"
#> 
#> [[6]]
#> [1] "NO, not reject NULL"
#> 
#> [[7]]
#> [1] "test_type:"
#> 
#> [[8]]
#> [1] " Two Sample t-test"
plot(ans1)
```


## Sample 2

```r
set.seed(32); x=rnorm(30,mean=10,sd=5)
set.seed(35); y=rnorm(30,mean=8,sd=15)
ans2=ADVTTEST::myttest(x,y,alpha=0.05,paired=FALSE)
print(ans2)
#> [[1]]
#> [1] "p_value:"
#> 
#> [[2]]
#> [1] 0.340561
#> 
#> [[3]]
#> [1] "CI:"
#> 
#> [[4]]
#> [1] -8.672098  3.086584
#> attr(,"conf.level")
#> [1] 0.95
#> 
#> [[5]]
#> [1] "Y/N"
#> 
#> [[6]]
#> [1] "NO, not reject NULL"
#> 
#> [[7]]
#> [1] "test_type:"
#> 
#> [[8]]
#> [1] "Welch Two Sample t-test"
```


## Sample 3

```r
set.seed(32); x=rnorm(30,mean=10,sd=15)
set.seed(35); y = x+ rnorm(30, 5 ,4)
ans3=ADVTTEST::myttest(x,y,alpha=0.05,paired=TRUE)
print(ans3)
#> [[1]]
#> [1] "p_value:"
#> 
#> [[2]]
#> [1] 1.29572e-09
#> 
#> [[3]]
#> [1] "CI:"
#> 
#> [[4]]
#> [1] -8.082480 -5.015335
#> attr(,"conf.level")
#> [1] 0.95
#> 
#> [[5]]
#> [1] "Y/N"
#> 
#> [[6]]
#> [1] "Yes, reject NULL"
#> 
#> [[7]]
#> [1] "test_type:"
#> 
#> [[8]]
#> [1] "Paired t-test"
plot(ans3)
```

