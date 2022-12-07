#' Data_pre
#'
#' This function is used to pre-process the data for regression.
#' For variables that require categorical analysis, this function factors out the variable.
#' If the data distribution is too skewed, log improvement is used.
#' This function will return the changed data frame
#' @param ln_name Variable you want to ln
#' @param fc_name Variable you want to treat as factor
#' @param data Data waitted ti be compute
#'
#' @return A transformed dataframe
#' @export
#' @examples data("test")
#' ln_name <- c("AGE")
#' fc_name <- c("ASTHMA")
#' data <- test
#' data_pre(ln_name,fc_name,data)
#'
data_pre <- function(ln_name,fc_name,data){

  data[,ln_name] <- purrr::map_dfc(data[,ln_name],log)
  data[,fc_name] <- purrr::map_dfc(data[,fc_name],factor)
  data <- as.data.frame(data)
  return(data)
}
