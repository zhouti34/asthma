test_that("describe test", {
  x <- c("AGE")
  y <- c("ASTHMA")
  cov <- c("BMI")
   data("test")
   expect_equal(describe(x,y,cov,test)[[2]],summary(test[,x]) )
})

