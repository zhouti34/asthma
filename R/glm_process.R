#' Glm_press
#'
#' This function is used to fit the linear relationship between variables
#' generalize linear model  an extension of the linear model that
#' mathematical expectations of the response variables and the predictor variables of the linear combination are established by a linkage function
#' The relationship between the mathematical expectation of the response variable and the linear combination of the predictor variables.
#' It is an extension of the linear model in studying the non-normal
#' distribution of response values and the concise and direct linear transformation of nonlinear models.
#' @import purrr stats
#'
#' @param x Independent variable
#' @param y Dependentvariable
#' @param cov Covariates
#' @param family Binomial or gaussian
#' @param data Data to be computed
#'
#' @return A result dataframe and regression curves with points of x,use plot() to draw plots
#'
#' @export
#'
#' @examples data("test")
#' y <- c("ASTHMA")
#' x <- c("AGE")
#' cov <- c("PA","BMI")
#' family <- "binomial"
#' data <- test
#' glm_process(x,y,cov,family,data)
#'
#' @references Henry L, Wickham H (2020). purrr: Functional Programming Tools. R package version 0.3.4, <https://CRAN.R-project.org/package=purrr>
#' @references R Core Team (2022). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. <https://www.R-project.org/>.



glm_process <- function(x,y,cov,family,data){

  Y <- paste0(y,collapse = "+")
  X <- paste0(x,collapse = "+")
  COV <- paste0(cov,collapse = "+")


  t <- paste0(Y,"~",X,"+",COV)
  t1 <- glm(t,family = family,data = data)

  t2 <- as.data.frame(summary(t1)$coefficients)
  data[,c("fv","L","U")] <- predict(t1,data,interval = "confidence")

 p <-  ggplot(data, mapping = aes(get(x), fv)) + geom_point() +
    geom_smooth(method="lm", se=T )+
    xlab(x)
 t3 <- list(as.data.frame(summary(t1)$coefficients),p)
 names(t3) <- c("result","plot")
 print(t3[[2]])
 return(t3)




}

