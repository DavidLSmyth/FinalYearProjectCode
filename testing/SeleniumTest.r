context("basic")

library(RSelenium)
library(testthat)


RSelenium::startServer()
appURL <- "http://127.0.0.1:6060"
browser <- "firefox"
version <- "26"
remDr <- remoteDriver$new(browserName=browser)
remDr$open()

test_that("can connect to app", {  
  remDr$navigate(appURL)
  appTitle <- remDr$getTitle()[[1]]
  expect_equal(appTitle, "Shiny Test App")  
})

remDr$close()