#' Plot One or Two Normal Density Functions
#'
#' Plots the normal density function of one or two graphs.
#'
#' @param mean Mean of the first distribution
#' @param sd Standard deviation of the first distribution
#' @param mean2 Mean of the second distribution
#' @param sd2 Standard deviation of the second distribution
#'
#' @return Density graphs
#'
#' @export
#' @importFrom stats dnorm
normal.plot <- function(mean=0,sd=1,mean2=NA,sd2=NA){
  storage <- matrix(c(mean,sd,mean2,sd2),nrow=2,ncol=2)
  x <- seq(from=min(storage[1,],na.rm=TRUE)-3*max(storage[2,],na.rm=TRUE),to=max(storage[1,],na.rm=TRUE)+3*max(storage[2,],na.rm=TRUE),length=1000)
  y1 <- dnorm(x,mean=storage[1,which.min(storage[2,])],sd=min(storage[2,],na.rm=TRUE))
  if(is.na(mean2)==FALSE) y2 <- dnorm(x,mean=storage[1,which.max(storage[2,])],sd=max(storage[2,],na.rm=TRUE))
  plot(x,y1,type="l",xlab="Data Values",ylab="Density")
  if(is.na(mean2)==FALSE) lines(x,y2,col="red")
}
