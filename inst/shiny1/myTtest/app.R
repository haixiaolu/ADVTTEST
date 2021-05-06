#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# set up data
set.seed(32)
x = rnorm(30,mean=10,sd=15)
x1 = rnorm(30,mean=10,sd=5)
x2 = rnorm(30,mean=10,sd=15)

set.seed(35);
y=rnorm(30,mean=8,sd=15)
y1=rnorm(30,mean=8,sd=15)
y2 = x+ rnorm(30, 5 ,4)

# make a data frame
df = data.frame(x, x1, x2, y, y1, y2)
names(df) <- c('x', 'x1', 'x2', 'y', 'y1', 'y2')

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
                #h3(textOutput("Summary", container = span)),
                verbatimTextOutput("summary")
            )
    )
 )


# Define server logic required to draw a boxplot

server <- function(input, output) {

    selectedData <- reactive({
        df[,c(input$xcol, input$ycol)]
    })

    output$boxPlot <- renderPlot({

        pop = rep(c("x", "y"), c(length(df$x), length(df$y)))
        # make a data frame
        df1 = data.frame(pop = pop,
                        samples = c(df$x, df$y))
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
# Run the application
shinyApp(ui = ui, server = server)
