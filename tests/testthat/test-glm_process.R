test_that("multiplication works", {
 data("test")
  x <- "AGE"
  y <- "ASTHMA"
  cov <- "GENDER"
  t1 <- glm_process(x,y,cov,data = test,family = "binomial")
  expect_equal(is.data.frame(t1[[1]]),TRUE)

})
