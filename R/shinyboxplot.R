#' Shiny boxplot App
#'
#' @description This function calls the Shiny Web Application on browser
#'
#' @details Use this function to display the boxplots and statistic summary on web browser
#'
#' @return Different boxplots and Statistic summary based on t.test type
#'
#' @keywords shiny boxplot geom_boxplot
#'
#' @export
#'
#' @examples
#' \dontrun{shinyboxplot()}
shinyboxplot<-function(){
    shiny::runApp(system.file("inst/shiny1/myTtest", package = "ADVTTEST"),launch.browser = TRUE)
}
