#' adjust
#'
#' This function is used to pre-process the data for regression.
#' For variables that require categorical analysis, this function factors out the variable.
#' log improvement is used for the skewed data distribution.
#' This function will return the changed data frame
#' @param ln_name Variable you want to ln
#' @param fc_name Variable you want to treat as factor
#' @param data Data waited to be computed
#'
#' @import purrr
#'
#' @return A transformed data frame
#'
#' @export
#'
#' @examples data("asthmaSurvey")
#' ln_name <- c("AGE")
#' fc_name <- c("ASTHMA")
#' data <- asthmaSurvey
#' adjust(ln_name,fc_name,data)
#'
adjust <- function(ln_name,fc_name,data){

  data[,ln_name] <- purrr::map_dfc(data[,ln_name],log)
  data[,fc_name] <- purrr::map_dfc(data[,fc_name],factor)
  data <- as.data.frame(data)
  return(data)
}

