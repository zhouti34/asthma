#' Histogram_plot
#'
#' This function serves to draw a histogram of the variables
#'A histogram is an exact graphical representation of the distribution of numerical data.
#'It is an estimate of the probability distribution of a continuous variable (quantitative variable).
#'The first step is to segment the range of values
#'These values are usually specified as continuous, non-overlapping intervals of the variable.
#'The intervals must be adjacent and usually (but not necessarily) of equal size
#' @import ggplot2
#' @param x The variable you want to plot
#' @param title Plot's title
#' @param xlab  Name of xlab
#' @param ylab  Name of ylab
#' @param  data to be compute
#'
#' @return A histogram plot
#' @export
#' @examples data("test")
#'y <- c("ASTHMA")
#'x <- c("AGE")
#'data <- test
#' his_plot(x,"title","xlab","ylab",data)
#'

his_plot <- function(x,title,xlab,ylab,data){

  p1<-
    ggplot(data = data,aes(x=get(x)))+
    geom_histogram(aes(y=..density..),bins = 60,fill="blue",alpha=0.6,colour="black")+
    geom_density(size=1)+
    ggtitle(title)+
    xlab(xlab)+
    ylab(ylab)
  p1
}


