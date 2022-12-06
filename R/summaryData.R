#' summaryData
#'
#' This function is used to describe
#' the basic situation of the variables in
#' the data set, such as the maximum and minimum values,
#' the 25th quartile and the 75th quartile.
#' A summary list is also returned in the result,
#' and this table shows the detailed distribution of the
#' variables entered. Variables need to be specified as
#' independent, dependent and covariate variables
#' The data set contains the prevalence of asthma and some
#' basic information about the population such as
#' BMI, activity level (PA), age, gender, race, alcohol use (ALQ), and smoking status (COT).
#'
#' @name  summaryData
#'
#' @param ind Independent variable
#' @param dep Dependentvariable
#' @param cov Covariates
#' @param data Dataset you want to describe,must be a dataframe
#'
#' @return  A table in Viewwe windows,and summary statistics of variables
#'
#' @export
#'
#' @examples data("asthmaSurvey")
#' dep <- c("ASTHMA")
#' ind <- c("AGE")
#' cov <- c("PA","BMI")
#' data <- asthmaSurvey
#' summaryData(ind,dep,cov,data)





summaryData <- function(ind,dep,cov,data){
  Y <- paste0(dep,collapse = "+")
  X <- paste0(ind,collapse = "+")
  COV <- paste0(cov,collapse = "+")
  t1 <- paste0("~",Y,"+",X,"+",COV)
  t <-  table1::table1(as.formula(t1),data=data)
  t2 <- summary(data[,ind])
  t3 <- summary(data[,dep])
  t4 <- summary(data[,cov])
  all_summary <- list(t,t2,t3,t4)
  names(all_summary) <- c("TABLE1","X_SUMMARY","Y_SUMMARY","COV_SUMMARY")
  return(all_summary)
}


