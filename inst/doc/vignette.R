## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(asthma)
data("test")
y <- c("ASTHMA")
x <- c("AGE")
cov <- c("PA","BMI")
cor_name <- c("PA","BMI","AGE")
ln_name <- c("AGE")
fc_name <- c("ASTHMA")
family <- "binomial"

data <- test

## ----describe-----------------------------------------------------------------
describe(x,y,cov,data)

## ----data_pre-----------------------------------------------------------------
data_pre(ln_name,fc_name,data)

## ----his_plot-----------------------------------------------------------------
his_plot(x,"title","xlab","ylab",data)

## ----cor_plot-----------------------------------------------------------------
cor_plot(cor_name,data)

## ----glm_process--------------------------------------------------------------
glm_process(x,y,cov,family,data)

