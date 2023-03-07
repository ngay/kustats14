#' Permutation
#'
#' @param n Number of objects being picked from
#' @param r Number of objects to be ordered
#' @return The number of ways to select r objects, in order, from n objects
#' @export
permutation <- function(n,r) {
  if (n < r) {
    return(0)
  } else {
    a = seq(from=n,to=n-r+1)
    return(prod(a))
  }
}
