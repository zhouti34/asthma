test_that("data_pre", {
 data("test")
  ln_name<- c("AGE")
  fc_name <- c("ASTHMA")
  t1 <- data_pre(ln_name ,fc_name,data=test)
  expect_equal(is.factor(t1[,fc_name]),isTRUE(1==1))
})
