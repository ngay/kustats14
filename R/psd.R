#' Population Standard Deviation
#'
#' @param x A data set
#'
#' @return Population standard deviation
#' @export
#' @importFrom stats na.omit var
psd = function(x) {
  n = length(na.omit(x))
  return(sqrt(var(x)*(1-1/n)))
}
