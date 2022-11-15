#' Cor_plot
#'
#' This function is used to fit the correlation between variables
#'The Spearman's correlation coefficient indicates the direction of correlation between X (the independent variable) and Y (the dependent variable). If
#'If Y tends to increase when X increases, the Spearman's correlation coefficient is positive. If Y tends to decrease when X
#'increases, Y tends to decrease, the Spearman correlation coefficient is negative. Spearman's correlation
#'A zero coefficient indicates that there is no tendency for Y to increase when X increases. As X and Y get closer to complete
#'The Spearman correlation coefficient increases in absolute value as X and Y become closer to complete monotonic correlation. When X and Y
#'are completely monotonically correlated, the absolute value of the Spearman correlation coefficient is 1.
#'This function returns a plot of the total correlation, with the size and color of the circles showing the correlation
#'
#' @import  corrplot ggplot2 ggcorrplot stats grDevices
#' @param cor_name Variables you want to make corplot
#' @param data Data waitted ti be compute
#' @return Cor_plot
#' @export
#' @examples data("test")
#' data <- test
#' cor_name <- c("PA","BMI","AGE")
#' cor_plot(cor_name,data)
#' @references H. Wickham. ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag New York, 2016.
#' @references Taiyun Wei and Viliam Simko (2021). R package 'corrplot': Visualization of a Correlation Matrix (Version 0.92). Available from <https://github.com/taiyun/corrplot>
#' @references Kassambara A (2022). ggcorrplot: Visualization of a Correlation Matrix using 'ggplot2'. R package version 0.1.4, <https://CRAN.R-project.org/package=ggcorrplot>.
#' @references R Core Team (2022). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL <https://www.R-project.org/>.


cor_plot <- function(cor_name,data){
  tmp <- data.matrix(data[,cor_name])
  COR_X <- cor(tmp,method = "spearman")
  pmat <- cor_pmat(tmp)
  col3 <- colorRampPalette(c(hex="#FF4500","#00CED1"))
  corrplot(COR_X,
           order = "AOE",
           type = "upper",
           tl.pos = "d",
           tl.cex = 0.9,
           col = col3(30),
           number.cex = .3,
           tl.col="#2F4F4F",
           p.mat = pmat,
           pch=3,
           pch.col = "#FF3030",
           pch.cex = 1)
  corrplot(
    COR_X,
    add = TRUE,
    type = "lower",
    method = "number",
    col = col3(10),
    order = "AOE",
    diag = F,
    tl.pos = "n",
    cl.pos = "n"
  )

}
