#' @include aFunction.R
NULL

#' Multiplies two numbers, adds a constant, and returns the result
#' @export
#' @title Multiplies two numbers, adds a constant, and returns the result
#' @name anotherFunction
#' @inheritParams aFunction
#' @param c A constant to add
#' @return A number - the product of a and b, plus c
#' @author Paul Newcombe
#' @example Examples/AnotherFunctionExample.R
anotherFunction <- function(a, b, c) {
  result <- aFunction(a, b)
  result <- result*c
  return(result)
}