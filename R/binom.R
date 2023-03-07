#' Binomial Probabilities
#'
#' A wrapper function for dbinom.
#'
#' @param x Number of successes
#' @param p Probability of a success on each trial
#' @param n The number of trials
#' @return The binomial probability of seeing x successes out of n trials when the probability of a success is p.
#' @importFrom stats dbinom
#' @export
#'
binom <- function(n,x,p){
  return(dbinom(x=x,prob=p,size=n))
}
