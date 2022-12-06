#' Histogram_plot
#'
#' This function serves to draw a histogram of the variables
#' A histogram is an exact graphical representation of the distribution of numerical data.
#' It is an estimate of the probability distribution of a continuous variable (quantitative variable) that
#' The first step is to segment the range of values
#' These values are usually specified as continuous, non-overlapping intervals of the variable.
#' The intervals must be adjacent and usually (but not necessarily) of equal size
#' @import ggplot2
#'
#' @param x Character vector specifying the variable from the dataset to be plot. Example "AGE"
#' @param title the title of the histograms
#' @param data to be computed
#'
#' @return A histogram plot of
#'
#' @export
#'
#' @examples data("asthmaSurvey")
#' y <- c("ASTHMA")
#' x <- c("AGE")
#' data <- asthmaSurvey
#' his_plot(x,"The Histogram plot",data)

#' @references H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016.

his_plot <- function(x,title,data){

  p1<-
    ggplot(data = data,aes(x=get(x)))+
    geom_histogram(aes(y=..density..),bins = 60,fill="blue",alpha=0.6,colour="black")+
    geom_density(size=1)+
    ggtitle(title)+
    xlab(paste(x))+
    ylab("frequency")
  p1
}



